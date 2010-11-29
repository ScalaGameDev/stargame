function setupSliders() {
  var nSliders = $('.research-slider').length;
  
  $('.research-slider').slider({
    range: "min",
    min: 0,
    max: 1000,
    slide: function(event, ui) {
      var oldValue = $(event.target).slider("option", "value");
      var newValue = ui.value;
      
      var delta = newValue-oldValue;
      
      $('.research-slider').each(function(i, elem) {
        if(elem != ui.handle) {
          var elemOldValue = $(elem).slider("option", "value");
          $(elem).slider("option", "value", elemOldValue - delta/(nSliders-1));
        }
      });
    }
  });
};

function getSliderVals() {
  return $.map($('.research-slider'), function(elem) { 
    return $(elem).slider("option", "value")/1000.0; 
  });
};

function setSliderVals(allocs) {
  var i = 0;
  var sliders = $('.research-slider');
  for(i = 0; i < sliders.length; i++) {
    $(sliders[i]).slider("option", "value", allocs[i]*1000.0);
  }
}

var playerInfo = null;
function setMyPlayer(playerInfoStr) {
  playerInfo = JSON.parse(playerInfoStr);
}

var mapView = null; 
function setMapView(mapViewStr) {
  mapView = JSON.parse(mapViewStr);
  drawMap();
}

var mapViewPort = null;
function getMapViewPort() {
  if(mapViewPort == null) {
    mapViewPort = new Object();
    var homeStar = mapView.starViews[playerInfo.exploredStarIds[0]];
    mapViewPort.x = homeStar.x;
    mapViewPort.y = homeStar.y;
    mapViewPort.PixelsPerLy = 20.0;
  }
  return mapViewPort;
}

var classToFillStyle = {
  "Giant": "rgb(255, 97, 81)",
  "B": "rgb(202, 215, 255)",
  "A": "rgb(248, 247, 255)",
  "F": "rgb(252, 255, 211)",
  "G": "rgb(255, 242, 161)",
  "K": "rgb(255, 163, 81)",
  "M": "rgb(255, 97, 81)",
  "Compact": "rgb(202, 215, 255)"
}

var playerIdToColor = {
  0: '#ff0000',
  1: '#0000ff',
  2: '#00ff00',
  3: '#ffff00',
  4: '#00ffff',
  5: '#ff00ff'
}

function drawMap() {
  var port = getMapViewPort();
  var canvas = $('#map-canvas')[0];
  var h = canvas.height;
  var w = canvas.width;
  var ctx = canvas.getContext('2d');
  ctx.font ='bold 12px Arial serif';
  
  // draw background
  ctx.fillStyle = 'rgb(0,0,0)';
  ctx.fillRect(0,0,w,h);
  
  function inViewPort(sv) {
    return Math.abs(port.x-sv.x)*port.PixelsPerLy < canvas.width/2 &&
           Math.abs(port.y-sv.y)*port.PixelsPerLy < canvas.height/2
  }
  
  function drawStarView(sv) {
    var pix_x = (sv.x - port.x)*port.PixelsPerLy + canvas.width/2
    var pix_y = (sv.y - port.y)*port.PixelsPerLy + canvas.height/2
    
    ctx.beginPath();
    ctx.fillStyle = classToFillStyle[sv.sClass];
    ctx.arc(pix_x, pix_y, 3.0, 0, 2*Math.PI, true);
    ctx.fill();
    
    if(sv.name) {
      var textC = null;
      if(sv.knownColonyOwnerId != undefined) {
        textC = playerIdToColor[sv.knownColonyOwnerId];
      } else {
        textC = '#ffffff';
      }
      ctx.fillStyle = textC;
      ctx.fillText(sv.name, pix_x-10, pix_y+15);
    }
  }
  
  // draw all qualifying stars
  mapView.starViews.filter(inViewPort).map(drawStarView);
}
