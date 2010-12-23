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
    var canvas = $('#map-canvas')[0];
    var homeStar = mapView.starViews[playerInfo.player.exploredStarIds[0]];
    
    var bounds = mapView.mapBounds;
    var bTol = 3.0;
    
    var startPply = 25.0;
    
    mapPort = {
      PixelsPerLy: startPply,
      x: homeStar.x - canvas.width/2/startPply,
      y: homeStar.y - canvas.height/2/startPply,
      entities: [], // all displayed entities
      selectedEuid: undefined,
      selectedEntity: function() {
        return this.entities.filter(function(ent) { 
          return ent.euid === mapPort.selectedEuid; 
        })[0]; 
      },
      xRight: function() { 
        return this.x+canvas.width/this.PixelsPerLy;
      },
      yBottom: function() {
        return this.y+canvas.height/this.PixelsPerLy; 
      },
      normalizePos: function() {
        var b = bounds;
        var c = canvas;
        var pply = this.PixelsPerLy;
        if( c.width/pply > b.xRight-b.xLeft + 2*bTol ) {
          // if zoomed out enough so that both left/r b
          // can be violated, just center the map to prevent thrashing
          this.x = (b.xRight+b.xLeft)/2 - c.width/2/pply;
        } else if(this.x < b.xLeft - bTol) {
          this.x = b.xLeft - bTol;
        } else if(this.xRight() > b.xRight + bTol) {
          this.x = b.xRight + bTol - c.width/pply;
        } 
        
        if( c.height/pply > b.yBottom-b.yTop + 2*bTol ) {
          this.y = (b.yBottom+b.yTop)/2 - c.height/2/pply;
        } else if(this.y < b.yTop - bTol) {
          this.y = b.yTop - bTol;
        } else if(this.yBottom() > b.yBottom + bTol) {
          this.y = b.yBottom + bTol - c.height/pply;
        }
      },
      doNormalizedZoom: function(zoomIn, zoomToX, zoomToY) {
        var multiplier = 1.0;
        if(zoomIn) { 
          // prevent zooming in past a threshhold
          if(this.PixelsPerLy > 60.0) { return; } else { multiplier = 1.2; }
        } else {
          // prevent zooming out past a threshold
          var b = bounds;
          // if statement tests if map scale already 'big enough'
          if( canvas.width/this.PixelsPerLy > b.xRight-b.xLeft + 2*bTol ||
              canvas.height/this.PixelsPerLy > b.yBottom-b.yTop + 2*bTol )
          { return; }
          else { multiplier = 1/1.2; }
        }
        
        this.PixelsPerLy *= multiplier;
        this.x = zoomToX - (zoomToX-this.x)/multiplier;
        this.y = zoomToY - (zoomToY-this.y)/multiplier;
        
        // do position normalization on zoomout (so we don't go out of bounds)
        if(!zoomIn) {
          this.normalizePos();
        }
      }
    
    };
    
    mapPort.normalizePos();
    
    // select the home star on load
    clickEntities([{ 
      type: 'sv', 
      obj: homeStar 
    }]);
  }
  return mapPort;
  
}

function starViewName(sv) {
  if(sv.name !== undefined) {
    return sv.name;
  } else {
    return "Unexplored star";
  }
}

function pixelToSpace(px, py) {
  return [mapPort.x + px/mapPort.PixelsPerLy, 
          mapPort.y + py/mapPort.PixelsPerLy];
}

function spaceToPixel(x, y) {
  return [(x-mapPort.x)*mapPort.PixelsPerLy, 
          (y-mapPort.y)*mapPort.PixelsPerLy];
}

function dist(a, b) {
  return Math.sqrt(Math.pow(a.x-b.x, 2) + Math.pow(a.y-b.y,2)); 
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
  
  selector.mousedown(function(e) {
    var [startPx, startPy] = myCurPos(e);
    
    if(startAction !== undefined) { 
      startAction(startPx, startPy); 
    }
    
    var dragged = false;
    
    var moveHandle = function(moveEvent) {
      var [px, py] = myCurPos(moveEvent);
      if(!dragged && Math.abs(px-startPx)+Math.abs(py-startPy) > 4) {
        dragged = true;
      }
      return action(px, py); 
    };
    
    selector.bind('mousemove', moveHandle);
    
    $(document).bind('mouseup', function (upEvent) {
      selector.unbind('mousemove');
      $(document).unbind('mouseup');
      
      if(finishAction !== undefined) { 
        var [endPx, endPy] = myCurPos(upEvent);
        finishAction(endPx, endPy, dragged); 
      }
    });
    
    // faux move on start.
    moveHandle(e);
  });
}

function selectSidebar(name) {
  $('.sidebar').hide();
  $('#'+name).show();
}

function drawLineInSpace(ctx, style, fromObj, toObj) {
  ctx.strokeStyle = style;
  
  ctx.beginPath();
  ctx.moveTo.apply(ctx, spaceToPixel(fromObj.x, fromObj.y));
  ctx.lineTo.apply(ctx, spaceToPixel(toObj.x, toObj.y));
  ctx.closePath();
  ctx.stroke();
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
  
  function makeEntity(euid, px, py, xtol, ytol, type, obj) {
    return {
      euid: euid,
      plft: px-xtol, 
      prht: px+xtol,
      ptop: py-ytol, 
      pbot: py+ytol,
      type: type,
      obj: obj
    };
  }
  
  function drawStarView(sv) {
    var [px,py] = spaceToPixel(sv.x, sv.y);
    
    ctx.beginPath();
    ctx.fillStyle = classToFillStyle[sv.sClass];
    ctx.arc(px, py, 10.0, 0, 2*Math.PI, true);
    ctx.fill();
    
    if(sv.name) {
      var textC = null;
      if(sv.knownColonyOwnerId !== undefined) {
        textC = playerIdToColor[sv.knownColonyOwnerId];
      } else {
        textC = '#ffffff';
      }
      ctx.fillStyle = textC;
      ctx.fillText(sv.name, px-10, py+25);
    }
    
    mapPort.entities.push(
      makeEntity("sv-"+sv.id.toString(), px, py, 14, 14, 'sv', sv));
  }
  
  function drawFleetView(fv) {
    var [px,py] = spaceToPixel(fv.x, fv.y);
    
    if(!fv.moving) {
      px+=20;
      py-=15;
    }
    
    ctx.beginPath();
    ctx.fillStyle = playerIdToColor[fv.playerId];
    ctx.moveTo(px-6, py-6);
    ctx.lineTo(px+10, py);
    ctx.lineTo(px-6, py+6);
    ctx.lineTo(px-3, py);
    ctx.lineTo(px-6, py-6);
    ctx.fill();
    
    if(fv.moving) {
      var toStarView = mapView.starViews[fv.toStarId];
      drawLineInSpace(ctx, 'rgba(255,255,255,0.8)', fv, toStarView);
    }
    
    mapPort.entities.push(
      makeEntity("fv-"+fv.uuid, px, py, 12, 8, 'fv', fv));
  }
  
  // draw all qualifying stars and fleets
  var visibleStarViews = mapView.starViews.filter(inViewPort); 
  visibleStarViews.map(drawStarView);
  mapView.fleetViews.filter(inViewPort).map(drawFleetView);
  
  // draw selected box  
  var e = mapPort.selectedEntity();
  if(e !== undefined) {
    ctx.fillStyle = "rgba(255,255,255,0.3)";
    ctx.fillRect(e.plft, e.ptop, e.prht-e.plft, e.pbot-e.ptop);
    ctx.strokeStyle = "rgba(255,255,255,1.0)";
    ctx.strokeRect(e.plft, e.ptop, e.prht-e.plft, e.pbot-e.ptop);
    
    // if stationary fleet selected, draw possible paths
    if(e.type == 'fv' && !e.obj.moving) {
      var inRangeStarViews = visibleStarViews.filter(function(sv) { 
        return dist(e.obj, sv) < playerInfo.shipRange; });
      
      inRangeStarViews.map(function(sv) {
        drawLineInSpace(ctx, "rgba(255,255,255, 0.2)", e.obj, sv);
      });
    }
  }
}
