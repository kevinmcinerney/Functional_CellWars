//function move(json,x,y,move){
//
//      $.ajax({
//        type: "GET",                                                        // GET or POST
//        url: 'http://localhost:9000/move/'+json+'/'+x+'/'+y+'/'+move        // Path to file
//        timeout: 2000,                                                      // Waiting time
//        success: function(json) {                                           // Show content
//          build(data)
//        },
//      });
//};

function build(json){

 gridData = json;

    $( "#json-grid")
    .text(JSON.stringify(gridData))
    .hide();

    var grid = d3.select("#grid")
    	.append("svg")
    	.attr("width","800px")
    	.attr("height","800px");

    var row = grid.selectAll(".row")
    	.data(gridData)
    	.enter().append("g")
    	.attr("class", "row");

    var column = row.selectAll(".square")
    	.data(function(d) { return d; })
    	.enter().append("rect")
    	.attr("class","square")
    	.attr("class", function(d) {
    	    if (d.team == 1){
                return "blank"}
            else if (d.team == 2){
                return "blank"}
            else if (d.team == 3){
                return "center"}
             else if (d.team == 4){
                return "center"}
            else
                return "#fff"
    	})
    	.attr("x", function(d) { return d.x; })
    	.attr("y", function(d) { return d.y; })
    	.attr("width", function(d) { return d.width; })
    	.attr("height", function(d) { return d.height; })
    	.style("fill", function(d) {
            if (d.team == 1){
                return "#e82cb9"}
            else if (d.team == 2){
                return "#efe92f"}
            else if (d.team == 3){
                return "black"}
             else if (d.team == 4){
                return "black"}
            else
                return "#fff"
        })
    	.style("stroke", "#222")
        .on('click', function(d) {
           var x = $(this).attr('x');
           var y = $(this).attr('y');
           var json = JSON.stringify({"x":x,"y":y});
           var data = "["+json+","+$('#json-grid').text()+"]"
           alert(data);
           //move(data)
           });

};


$.getJSON('http://localhost:9000/startPositions/40', function(json){

   build(json)

});





