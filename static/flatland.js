(function () {
  var grid = {
    renderGrid: function (tiles, size, scale) {
      d3.select("#grid")
        .selectAll("li")
        .data(tiles)
        .style("left", function (d, i) { return (i % size) * scale + "px"; })
        .style("top",  function (d, i) { return Math.floor(i / size) * scale + "px"; })
        .attr("class", function (d, i) { return "tile-" + (d & 0xf); })
        .enter()
        .append("li");
    },

    renderEntities: function (entities, size, scale) {
      d3.select("main")
        .selectAll("div")
        .data(entities)
        .attr("class", "entity")
        .style("left",  function (entity) { return (entity.position[0] - 0.5 + (size / 2)) * scale + "px"; })
        .style("top", function (entity) { return (entity.position[1] - 0.5 + (size / 2)) * scale + "px"; })
        .enter()
        .append("div");
    }
  };

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

    grid.renderGrid(data.tiles, size, scale);
    grid.renderEntities(entities, size, scale);
  }
}());
