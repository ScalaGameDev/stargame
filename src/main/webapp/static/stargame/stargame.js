var playerInfo = null;
function setMyPlayer(playerInfoStr) {
  playerInfo = JSON.parse(playerInfoStr);
}

var mapView = null; // all the visible entities
function setMapView(mapViewStr) {
  mapView = JSON.parse(mapViewStr);
  drawMap();
}

var mapPort = null; // map view settings
function initMapPort() {
  if(mapPort === null) {
    mapPort = {};
    var canvas = $('#map-canvas')[0];
    
    var homeStar = mapView.starViews[playerInfo.exploredStarIds[0]];
    
    mapPort.PixelsPerLy = 30.0;
    
    mapPort.x = homeStar.x - canvas.width/2/mapPort.PixelsPerLy;
    mapPort.y = homeStar.y - canvas.height/2/mapPort.PixelsPerLy;
    
    selectEntity({ type: 'sv', obj: homeStar });
    
    mapPort.selectedEntity = function() {
      return mapPort.entities.filter(function(ent) { 
        return ent.obj === mapPort.selectedObj;
      })[0];
    };
    
    mapPort.entites = []; // all displayed entities
  }
  return mapPort;
}

function pixelToSpace(px, py) {
  return [mapPort.x + px/mapPort.PixelsPerLy, 
          mapPort.y + py/mapPort.PixelsPerLy];
}

function spaceToPixel(x, y) {
  return [(x-mapPort.x)*mapPort.PixelsPerLy, 
          (y-mapPort.y)*mapPort.PixelsPerLy];
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
};

var playerIdToColor = {
  0: '#ff0000',
  1: '#0000ff',
  2: '#00ff00',
  3: '#ffff00',
  4: '#00ffff',
  5: '#ff00ff'
};

function curPos(e, selector) {
  var offsets = selector.offset();
  var px = parseInt(e.pageX - offsets.left, 10);
  var py = parseInt(e.pageY - offsets.top, 10);
  return [px,py];
}

function setDragAction(selector, action, startAction, finishAction) {
  
  function myCurPos(e) { return curPos(e, selector); }
  
  selector.movehandle = function(e) { return action.apply(this, myCurPos(e)); };
  
  selector.mousedown(function(e) {
    if(startAction !== undefined) { startAction.apply(this, myCurPos(e)); }
    
    selector.bind('mousemove', selector.movehandle);
    
    $(document).bind('mouseup', function () {
      selector.unbind('mousemove');
      $(document).unbind('mouseup');
      
      if(finishAction !== undefined) { finishAction.apply(this, myCurPos(e)); }
    });
    
    // faux move on start.
    selector.movehandle(e);
  });
}

function drawMap() {
  initMapPort();
  var canvas = $('#map-canvas')[0];
  var h = canvas.height;
  var w = canvas.width;
  var ctx = canvas.getContext('2d');
  ctx.font ='bold 12px Arial serif';
  
  // clear map entites
  mapPort.entities = [];
  
  // draw background
  ctx.fillStyle = 'rgb(0,0,0)';
  ctx.fillRect(0,0,w,h);
  
  function inViewPort(itemWithPosition) {
    var [px,py] = spaceToPixel(itemWithPosition.x, itemWithPosition.y);
    return px < canvas.width && px > 0 &&
           py < canvas.height && py > 0;
  }
  
  function drawStarView(sv) {
    var [px,py] = spaceToPixel(sv.x, sv.y);
    
    ctx.beginPath();
    ctx.fillStyle = classToFillStyle[sv.sClass];
    ctx.arc(px, py, 4.0, 0, 2*Math.PI, true);
    ctx.fill();
    
    if(sv.name) {
      var textC = null;
      if(sv.knownColonyOwnerId !== undefined) {
        textC = playerIdToColor[sv.knownColonyOwnerId];
      } else {
        textC = '#ffffff';
      }
      ctx.fillStyle = textC;
      ctx.fillText(sv.name, px-10, py+15);
    }
    
    var tol = 10;
    mapPort.entities.push({
     plft: px-tol, 
     prht: px+tol,
     ptop: py-tol, 
     pbot: py+tol,
     type: 'sv',
     obj: sv
    });
  }
  
  // draw all qualifying stars
  mapView.starViews.filter(inViewPort).map(drawStarView);
  
  // draw selected box  
  ctx.fillStyle = "rgba(255,255,255,0.5)";
  var e = mapPort.selectedEntity();
  if(e !== undefined) {
    ctx.fillRect(e.plft, e.ptop, e.prht-e.plft, e.pbot-e.ptop);
  }
}

function selectEntity(e) {
  mapPort.selectedObj = e.obj;
  
  if(e.type === "sv") {
    if(e.obj.name !== undefined) {
      $('#star-name').html(e.obj.name);
    } else {
      $('#star-name').html("Unexplored star");
    }
    
    $('#star-class').html(e.obj.sClass);
  }
}

$(document).ready(function() {
  var dragStartX = 0, dragStartY = 0;
  setDragAction($('#map-canvas'), function(px,py) {
    mapPort.x = mapPort.x + (dragStartX-px)/mapPort.PixelsPerLy;
    mapPort.y = mapPort.y + (dragStartY-py)/mapPort.PixelsPerLy;
    
    drawMap();
    
    dragStartX = px;
    dragStartY = py;
    return false;
  },
  function(px,py) {
    dragStartX = px;
    dragStartY = py;
    return false;
  }, function(px,py) {return false;});
  
  $('#map-canvas').wheel(function(e, d) {
    var multiplier = 1.0;
    if(d > 0) { 
      multiplier = 1.2;
    } else {
      multiplier = 1/1.2;
    }
    
    // make sure that mouse point does not move on zoom...
    var mouseCoord = pixelToSpace.apply(this, curPos(e, $('#map-canvas')));
    
    mapPort.PixelsPerLy *= multiplier;
    mapPort.x = mouseCoord[0] - (mouseCoord[0]-mapPort.x)/multiplier;
    mapPort.y = mouseCoord[1] - (mouseCoord[1]-mapPort.y)/multiplier;
    
    drawMap();
    return false;
  });
  
  $('#map-canvas').click(function(event,d) {
    var [px,py] = curPos(event, $('#map-canvas'));
    
    function clicked(e) {
      return px > e.plft && px < e.prht && 
             py > e.ptop && py < e.pbot;
    }
    
    var clickedEntities = mapPort.entities.filter(clicked);
    
    if(clickedEntities.length > 0) {
      selectEntity(clickedEntities[0]);
    }
    
    drawMap();
    return false;
  });
});
