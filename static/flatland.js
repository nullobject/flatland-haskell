(function () {
  var grid = {
    renderGrid: function (tiles, size) {
      var scale = 20;

      d3
      .select("#grid")
      .selectAll("li")
      .data(tiles)
      .enter()
      .append("li")
      .style("left", function (d, i) { return (i % size) * scale + "px"; })
      .style("top",  function (d, i) { return Math.floor(i / size) * scale + "px"; })
      .style("width",  scale + "px")
      .style("height", scale + "px")
      .attr("class", function (d, i) { return "tile-" + (d & 0xf); });
    },

    renderEntities: function (entities, size) {
      var scale = 20;

      d3
      .select("main")
      .selectAll("div")
      .data(entities)
      .attr("class", "entity")
      .style("left",  function (entity) { return (entity.position[0] - 0.5 + (size / 2)) * scale + "px"; })
      .style("top", function (entity) { return (entity.position[1] - 0.5 + (size / 2)) * scale + "px"; })
      .style("width",  20 + "px")
      .style("height", 20 + "px")
      .enter()
      .append("div")
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

    // document.getElementsByTagName("main")[0].innerHTML = data.age;

    var size = Math.sqrt(data.tiles.length);
    grid.renderGrid(data.tiles, size);

    var entities = data.players.map(function (player) {
      return player.entity;
    });
    grid.renderEntities(entities, size);
  }
}());
