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
      debugContainer = new createjs.Container(),
      playfieldRendered = false,
      debugRendered = false;

    centreCanvas(canvas);

    debugContainer.alpha = 0.5;

    stage.addChild(playfieldContainer);
    stage.addChild(entitiesContainer);
    // stage.addChild(debugContainer);

    createjs.Ticker.addEventListener("tick", function() { stage.update(); });
    createjs.Ticker.useRAF = true;
    createjs.Ticker.setFPS(60);

    function updateDebug(rectangles, tileWidth, tileHeight, container) {
      if (debugRendered) return;
      debugRendered = true;
      return rectangles.map(function(rectangle) {
        var shape = new createjs.Shape()
        shape.graphics.beginFill("#ff0000").drawRect(
          rectangle.rectanglePosition[0] * tileWidth,
          rectangle.rectanglePosition[1] * tileHeight,
          rectangle.rectangleExtents[0] * tileWidth,
          rectangle.rectangleExtents[1] * tileHeight
        );
        container.addChild(shape);
        return shape;
      });
    }

    function updatePlayfield(layers, tileWidth, tileHeight, container) {
      if (playfieldRendered) return;
      playfieldRendered = true;
      return layers.map(function(layer, index) {
        var subContainer = new createjs.Container();
        container.addChild(subContainer);
        updateTiles(layer.tiles, tileWidth, tileHeight, subContainer);
        return subContainer;
      });
    }

    function updateTiles(tiles, tileWidth, tileHeight, container) {
      return tiles.map(function(tile, index) {
        var sprite = addTile(tile, container, spriteSheets.tiles),
          x = tile.position[0] * tileWidth,
          y = tile.position[1] * tileHeight;

        if (tile.dFlipped && tile.hFlipped && tile.vFlipped) {
          sprite.set({x: x, y: y, scaleY: -1, rotation: -90, regX: 16, regY: 16});
        } else if (tile.dFlipped && tile.hFlipped) {
          sprite.set({x: x, y: y, rotation: 90, regY: 16});
        } else if (tile.dFlipped && tile.vFlipped) {
          sprite.set({x: x, y: y, rotation: -90, regX: 16});
        } else if (tile.hFlipped && tile.vFlipped) {
          sprite.set({x: x, y: y, scaleX: -1, scaleY: -1, regX: 16, regY: 16});
        } else if (tile.dFlipped) {
          sprite.set({x: x, y: y, scaleX: -1, rotation: -90});
        } else if (tile.hFlipped) {
          sprite.set({x: x, y: y, scaleX: -1, regX: 16});
        } else if (tile.vFlipped) {
          sprite.set({x: x, y: y, scaleY: -1, regY: 16});
        } else {
          sprite.set({x: x, y: y});
        }

        sprite.gotoAndStop(tile.gid - 1);

        return sprite;
      });
    }

    function updateEntities(entities, tileWidth, tileHeight, container) {
      return entities.map(function(entity) {
        var sprite = container.getChildByName(entity.id) || addEntity(entity, container, spriteSheets.entities);
        sprite.gotoAndPlay(entity.state);
        sprite.setTransform(entity.position[0] * tileWidth, entity.position[1] * tileHeight);
        return sprite;
      });
    }

    var source = new EventSource("events");

    source.onopen = function() { console.log("open"); }
    source.onerror = function(e) { console.log(e); }

    source.onmessage = function(message) {
      var data = JSON.parse(message.data),
        entities = data
          .players
          .map(function(player) { return player.entity; })
          .filter(function(player) { return player != null });

      console.log(data);
      updateDebug(data.collisionRectangles, data.tileWidth, data.tileHeight, debugContainer);
      updatePlayfield(data.layers, data.tileWidth, data.tileHeight, playfieldContainer);
      updateEntities(entities, data.tileWidth, data.tileHeight, entitiesContainer);
    }
  });
}());
