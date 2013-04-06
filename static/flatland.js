function partial(fn) {
  var slice = Array.prototype.slice,
    args = slice.call(arguments, 1);

  return function() {
    return fn.apply(this, args.concat(slice.call(arguments)));
  };
}

(function() {
  function loadImage(url, callback) {
    var image = new Image();
    image.onload = function() { callback(null, image); }
    image.onerror = function() { callback("Couldn't load " + url); }
    image.src = url;
  }

  function loadEntities(callback) {
    loadImage("entities.png", function(error, image) {
      if (error) return callback(error);

      var spriteSheet = new createjs.SpriteSheet({
        images: [image],
        frames: [
          [30, 184, 22, 26, 0, 11, 13],
          [52, 184, 22, 26, 0, 11, 13],
          [74, 184, 22, 26, 0, 11, 13],
          [96, 184, 22, 26, 0, 11, 13]
        ],
        animations: {
          idle: {frames: [0]},
          moving: {frames: [1, 2, 3, 3, 2, 1], frequency: 10}
        }
      });

      callback(null, spriteSheet);
    });
  }

  function loadTiles(callback) {
    loadImage("tiles.png", function(error, image) {
      if (error) return callback(error);

      var spriteSheet = new createjs.SpriteSheet({
        images: [image],
        frames: {width: 16, height: 16}
      });

      callback(null, spriteSheet);
    });
  }

  function addTile(tile, stage, spriteSheet) {
    var sprite = new createjs.BitmapAnimation(spriteSheet);
    stage.addChild(sprite);
    return sprite;
  }

  function addEntity(entity, stage, spriteSheet) {
    var sprite = new createjs.BitmapAnimation(spriteSheet);
    stage.addChild(sprite);
    sprite.name = entity.id;
    return sprite;
  }

  function centreCanvas(canvas) {
    var parentWidth = canvas.parentNode.clientWidth,
      parentHeight = canvas.parentNode.clientHeight;
    canvas.style.top = (parentHeight - canvas.height) / 2 + "px";
    canvas.style.left = (parentWidth - canvas.width) / 2 + "px";
  }

  async.parallel({
    entities: loadEntities,
    tiles:    loadTiles
  }, function(error, spriteSheets) {
    if (error) return;

    var canvas = document.getElementById("main"),
      stage = new createjs.Stage(canvas),
      tilesRendered = false;

    centreCanvas(canvas);

    createjs.Ticker.addEventListener("tick", function() { stage.update(); });
    createjs.Ticker.useRAF = true;
    createjs.Ticker.setFPS(60);

    function updateTiles(tiles, size, scale) {
      if (tilesRendered) return;
      tilesRendered = true;
      return tiles.map(function(tile, index) {
        var sprite = addTile(tile, stage, spriteSheets.tiles);
        sprite.gotoAndStop(tile - 1);
        sprite.setTransform(
          Math.floor(index / size) * scale,
          (index % size) * scale
        );
        return sprite;
      });
    }

    function updateEntities(entities, size, scale) {
      return entities.map(function(entity) {
        var sprite = stage.getChildByName(entity.id) || addEntity(entity, stage, spriteSheets.entities);
        sprite.gotoAndPlay(entity.state);
        sprite.setTransform(
          entity.position[0] * scale,
          entity.position[1] * scale
        );
        return sprite;
      });
    }

    var source = new EventSource("events");

    source.onopen = function() { console.log("open"); }
    source.onerror = function(e) { console.log(e); }

    source.onmessage = function(message) {
      var data = JSON.parse(message.data),
        size = Math.sqrt(data.tiles.length),
        scale = 16,
        entities = data
          .players
          .map(function(player) { return player.entity; })
          .filter(function(player) { return player != null });

      updateTiles(data.tiles, size, scale);
      updateEntities(entities, size, scale);
    }
  });
}());
