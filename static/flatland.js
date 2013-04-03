(function () {
  // var grid = {
  //   renderGrid: function (tiles, size, scale) {
  //     d3.select("#grid")
  //       .selectAll("li")
  //       .data(tiles)
  //       .style("left", function (d, i) { return (i % size) * scale + "px"; })
  //       .style("top",  function (d, i) { return Math.floor(i / size) * scale + "px"; })
  //       .attr("class", function (d, i) { return "tile-" + (d & 0xf); })
  //       .enter()
  //       .append("li");
  //   },

  //   renderEntities: function (entities, size, scale) {
  //     d3.select("main")
  //       .selectAll("div")
  //       .data(entities)
  //       .attr("class", "entity")
  //       .style("left",  function (entity) { return (entity.position[0] - 0.5 + (size / 2)) * scale + "px"; })
  //       .style("top", function (entity) { return (entity.position[1] - 0.5 + (size / 2)) * scale + "px"; })
  //       .enter()
  //       .append("div");
  //   }
  // };

  function partial(fn) {
    var slice = Array.prototype.slice,
      args = slice.call(arguments, 1);

    return function() {
      return fn.apply(this, args.concat(slice.call(arguments)));
    };
  }

  function loadImage(url, callback) {
    var image = new Image();
    image.onload = function () { callback(null, image); };
    image.onerror = function () { callback("Couldn't load " + url); };
    image.src = url;
  }

  function loadEntities(callback) {
    loadImage("entities.png", function (error, image) {
      if (error) return callback(error);

      var spriteSheet = new createjs.SpriteSheet({
        images: [image],
        // frames: {width: 18, height: 26, count:1, regX: 32, regY: 184},
        frames: [
          [30, 184, 22, 26, 0],
          [52, 184, 22, 26, 0],
          [74, 184, 22, 26, 0],
          [96, 184, 22, 26, 0]
          // [118, 184, 23, 26, 0],
          // [146, 184, 23, 26, 0],
          // [174, 184, 23, 26, 0],
          // [197, 184, 23, 26, 0]
        ],
        animations: {
          idle: {frames: [0]},
          walk: {frames: [1, 2, 3, 3, 2, 1], frequency: 10}
          // walk: {frames: [1, 0, 2, 2, 0, 1], frequency: 10}
        }
      });

      callback(null, spriteSheet);
    });
  }

  function loadTiles(callback) {
    loadImage("tiles.png", function (error, image) {
      if (error) return callback(error);

      var spriteSheet = new createjs.SpriteSheet({
        images: [image],
        frames: {width: 16, height: 16}
      });

      callback(null, spriteSheet);
    });
  }

  function addTile(stage, spriteSheet, frame) {
    var tile = new createjs.BitmapAnimation(spriteSheet);
    stage.addChild(tile);
    tile.gotoAndStop(frame);
    return tile;
  }

  function addEntity(stage, spriteSheet, frame) {
    var entity = new createjs.BitmapAnimation(spriteSheet);
    stage.addChild(entity);
    entity.gotoAndPlay(frame);
    return entity;
  }

  function centreCanvas(canvas) {
    var parentWidth = canvas.parentNode.clientWidth;
    var parentHeight = canvas.parentNode.clientHeight;
    canvas.style.top = (parentHeight - canvas.height) / 2 + "px";
    canvas.style.left = (parentWidth - canvas.width) / 2 + "px";
  }

  async.parallel({
    entities: loadEntities,
    tiles:    loadTiles
  }, function(error, spriteSheets) {
    if (error) return;

    var canvas = document.getElementById("main");
    var stage = new createjs.Stage(canvas);

    centreCanvas(canvas);

    createjs.Ticker.addEventListener("tick", function () { stage.update(); });
    createjs.Ticker.useRAF = true;
    createjs.Ticker.setFPS(60);

    addEntity(stage, spriteSheets.entities, "idle").setTransform(0, 260);
    addEntity(stage, spriteSheets.entities, "walk").setTransform(0, 290);
    // addTile(stage, spriteSheets.tiles, 62).setTransform(32, 0);
    // addTile(stage, spriteSheets.tiles, 62).setTransform(48, 0);

    function updateTiles(tiles, size, scale) {
      tiles.map(function (tile, index) {
        var x = (index % size) * scale, y = Math.floor(index / size) * scale;
        addTile(stage, spriteSheets.tiles, (tile & 0xf) + 61).setTransform(x, y);
      });
    }

    var source = new EventSource("events");

    source.onopen = function() {
      console.log("open");
    }

    source.onerror = function(e) {
      console.log(e);
    }

    source.onmessage = function(message) {
      var data = JSON.parse(message.data);
      var size = Math.sqrt(data.tiles.length), scale = 16;
      var entities = data
        .players
        .map(function (player) { return player.entity; })
        .filter(function (player) { return player != null });

      updateTiles(data.tiles, size, scale);
    }
  });
}());
