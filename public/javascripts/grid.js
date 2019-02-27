function move(json){

      var obj = JSON.stringify(json)
      console.log(obj)

      $.ajax({
        url: 'http://localhost:9000/move',
        type: "POST",
        data: obj,
        contentType:"application/json;",
        dataType:"json",
        success: function(data){
          build(data, data[0].length)
        },
        error: function(request, status, error){
            alert(request.responseText)}
            })
};

function build(response, size){

    function gridData() {
        var data = new Array();
        var xpos = 1; //starting xpos and ypos at 1 so the stroke will show when we make the grid below
        var ypos = 1;
        var width = 600 / size;
        var height = 600 / size;
        var team = -1;

        console.log(response);

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
                    team: response[row][column]
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

    console.log(data);

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
    .enter()
    .append("rect")
    .attr("class","square")
    .attr("class", function(d) {
        if (d.team == 0){
            return "blank"}
        else if (d.team == 1){
            return "teamOne"}
        else if (d.team == 2){
            return "teamTwo"}
        else if (d.team == 3){
            return "TeamOneNuc"}
        else if (d.team == 6){
            return "teamTwoNuc"}
        else
            return "blank"
    })
    .attr("x", function(d) { return d.x; })
    .attr("y", function(d) { return d.y; })
    .attr("width", function(d) { return d.width; })
    .attr("height", function(d) { return d.height; })
    .style("fill", function(d) {
        if (d.team == 0){
            return "white"}
        else if (d.team == 1){
            return "red"}
        else if (d.team == 2){
            return "blue"}
        else if (d.team == 3){
            return "yellow"}
        else if (d.team == 6){
            return "green"}
        else
            return "white"
    })
    .style("stroke", "#222")
    .on('click', function(d) {
       var x = ($(this).attr('x') - 1) / $(this).attr('height');
       var y = ($(this).attr('y') - 1) / $(this).attr('height');
       if(d.team == 3 || d.team == 6){

           var opt = prompt("(1) Up, (2) Down, (3) Left, (4) Right");
           var choice;
           if(opt == 8){
               choice = "up"
           }else if(opt == 2) {
               choice = "down"
           }else if(opt == 4){
               choice = "left"
           }else{
               choice = "right"
           }
           move({"x":x,"y":y,"move": choice})
       }else{
            alert("Click on Cell center to make move")
       }

       });

};

function startPositions(size){

    $.getJSON('http://localhost:9000/startPositions/'+ size, function(json){

        build(json, size)

    })

};


startPositions(40)






