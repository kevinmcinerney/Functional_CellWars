function move(json, x, y, move){

      var obj = JSON.stringify(json)
      console.log(obj)

      $.ajax({
        url: 'http://localhost:9000/move',
        type: "POST",
        data: json,
        contentType:"application/json;",
        dataType:"json",
        success: function(data){
          build(data, 40)
        },
        error: function(msg){
        console.log(msg)}})
};

function build(response, size){

    function gridData() {
        var data = new Array();
        var xpos = 1; //starting xpos and ypos at 1 so the stroke will show when we make the grid below
        var ypos = 1;
        var width = 600 / size;
        var height = 600 / size;
        var team = -1;

        // iterate for rows
        for (var row = 0; row < size; row++) {
            data.push( new Array() );

            // iterate for cells/columns inside rows
            for (var column = 0; column < size; column++) {
                data[row].push({
                    x: xpos,
                    y: ypos,
                    width: width,
                    height: height,
                    team: team
                })
                // increment the x position. I.e. move it over by 50 (width variable)
                xpos += width;
            }
            // reset the x position after a row is complete
            xpos = 1;
            // increment the y position for the next row. Move it down 50 (height variable)
            ypos += height;
        }
        return data;
    }

    var data = gridData();

    console.log(response); //problem with response

    for(team in response.board.teamOne.cells){
        var c = response.board.teamOne.cells[team];
        x1 = c.topLeft.x;
        y1 = c.topLeft.y;
        x2 = c.botRight.x;
        y2 = c.botRight.y;
    	for (var row = x1; row < x2; row++) {
            for (var col = y1; col < y2; col++) {
                data[col][row].team = 0;
            }
        }
    }

    for(team in response.board.teamTwo.cells){
            var c = response.board.teamTwo.cells[team];
            x1 = c.topLeft.x;
            y1 = c.topLeft.y;
            x2 = c.botRight.x;
            y2 = c.botRight.y;
        	for (var row = x1; row < x2; row++) {
                for (var col = y1; col < y2; col++) {
                    data[col][row].team = 1;
                }
            }
        }



    console.log(data);

    $( "#json-grid")
    .text(JSON.stringify(response))
    .hide();

    var svg = d3.select('svg');
    svg.remove();

    var grid = d3.select("#grid")
    .append("svg")
    .attr("width","800px")
    .attr("height","800px");

    var row = grid.selectAll(".row")
    .data(data)
    .enter().append("g")
    .attr("class", "row");

    var column = row.selectAll(".square")
    .data(function(d) { return d; })
    .enter().append("rect")
    .attr("class","square")
    .attr("class", function(d) {
        if (d.team == 0){
            return "outer"}
        else if (d.team == 1){
            return "outer"}
        else if (d.team == 2){
            return "center"}
         else if (d.team == 3){
            return "center"}
        else
            return "#fff"
    })
    .attr("x", function(d) { return d.x; })
    .attr("y", function(d) { return d.y; })
    .attr("width", function(d) { return d.width; })
    .attr("height", function(d) { return d.height; })
    .style("fill", function(d) {
        if (d.team == 0){
            return "#e82cb9"}
        else if (d.team == 1){
            return "#efe92f"}
        else if (d.team == 2){
            return "black"}
         else if (d.team == 3){
            return "black"}
        else
            return "#fff"
    })
    .style("stroke", "#222")
    .on('click', function(d) {
       var x = ($(this).attr('x') - 1) / $(this).attr('height');
       var y = ($(this).attr('y') - 1) / $(this).attr('height');
       var json = $('#json-grid').text();
       alert("x= "+ x + " y= " + y + json)
       move(json, x, y, "down")
       });

};

function startPositions(size){

    $.getJSON('http://localhost:9000/startPositions/'+ size, function(json){

        build(json, size)

    })

};

startPositions(40)





