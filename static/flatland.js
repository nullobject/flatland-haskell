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

  function addTile(tile, container, spriteSheet) {
    var sprite = new createjs.BitmapAnimation(spriteSheet);
    container.addChild(sprite);
    return sprite;
  }

  function addEntity(entity, container, spriteSheet) {
    var sprite = new createjs.BitmapAnimation(spriteSheet);
    container.addChild(sprite);
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
      playfieldContainer = new createjs.Container(),
      entitiesContainer = new createjs.Container(),
      playfieldRendered = false;

    centreCanvas(canvas);

    stage.addChild(playfieldContainer);
    stage.addChild(entitiesContainer);

    createjs.Ticker.addEventListener("tick", function() { stage.update(); });
    createjs.Ticker.useRAF = true;
    createjs.Ticker.setFPS(60);

    function updatePlayfield(layers, scale, container) {
      if (playfieldRendered) return;
      playfieldRendered = true;
      return layers.map(function(layer, index) {
        var subContainer = new createjs.Container();
        container.addChild(subContainer);
        updateTiles(layer.tiles, scale, subContainer);
        return subContainer;
      });
    }

    function updateTiles(tiles, scale, container) {
      return tiles.map(function(tile, index) {
        var sprite = addTile(tile, container, spriteSheets.tiles);
        sprite.gotoAndStop(tile.gid - 1);
        sprite.setTransform(tile.position[0] * scale, tile.position[1] * scale);
        return sprite;
      });
    }

    function updateEntities(entities, scale, container) {
      return entities.map(function(entity) {
        var sprite = stage.getChildByName(entity.id) || addEntity(entity, container, spriteSheets.entities);
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
        scale = 16,
        entities = data
          .players
          .map(function(player) { return player.entity; })
          .filter(function(player) { return player != null });

      updatePlayfield(data.layers, scale, playfieldContainer);
      updateEntities(entities, scale, entitiesContainer);
    }
  });
}());
