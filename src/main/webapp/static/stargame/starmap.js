var mapView = null; // all the visible entities
function setMapView(mapViewStr) {  
  mapView = JSON.parse(mapViewStr);
  var playerInfo = mapView.playerInfo;
  // update player dashboard
  $('#playerInfoDashboard').html(
    "<table class='dashboard'>" + 
    "<tr>" +
      "<td>RU: " + playerInfo.player.gold.toFixed(1) + "</td>" +
      "<td>Sensor range: "   + playerInfo.sensorRange + "</td>" +
      "<td>Year: "   + mapView.gameYear.toFixed(1) + "</td>" +
    "</tr>" +
    "<tr>" +
      "<td>Ship Attack: " + 100 + "</td>" +
      "<td>Ship Speed: "  + playerInfo.speed + "</td>" +
      "<td>Ship Range: "  + playerInfo.range + "</td>" +
    "</tr>"+
    "</table>");
  
  drawMap();
  
  var pid = mapView.playerInfo.player.id;
  
  // generate battle reports html
  var rptDivs = mapView.lastReports.map(function(r) {
    var styleClass;
    var verb;
    if(pid == r.victorId) {
      verb = "Won";
      styleClass = "battlerpt battlewon";
    } else {
      verb = "Lost";
      styleClass = "battlerpt battlelost";
    }
    
    return "<div class='"+styleClass+"'><strong>" + verb + " battle over " +
         mapView.starViews[r.starId].name + "</strong><br/>" +
         "Victor: " + mapView.playerNames[r.victorId] + 
         " with " + r.shipsRemaining + " ships remaining" +
         "</div>";
  });
  
  $('#reports').html("<h2>Reports</h2>"+rptDivs.join("\n"));
  
  // refresh sidebar of what's selected
  if(mapPort !== null) {
    refreshEntitySelection();
  }
}

var mapPort = null; // map view settings
function initMapPort() {
  if(mapPort === null) {
    var canvas = $('#map-canvas')[0];
    var homeStar =
      mapView.starViews[mapView.playerInfo.player.exploredStarIds[0]];
    
    var bounds = mapView.mapBounds;
    var bTolPix = 60.0;
    
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
        var bTol = bTolPix/pply;
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
        var bTol = bTolPix/this.PixelsPerLy;
        if(zoomIn) { 
          // prevent zooming in past a threshhold
          if(this.PixelsPerLy > 60.0) { return; } else { multiplier = 1.2; }
        } else {
          // prevent zooming out past a threshold
          var b = bounds;
          // if statement tests if map scale already 'big enough'
          if( canvas.width/this.PixelsPerLy > b.xRight-b.xLeft + 2*bTol &&
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
    selectEntities([{ 
      type: 'sv', 
      euid: homeStar.euid,
      obj: homeStar 
    }]);
  }
  return mapPort;
  
}

function starViewName(sv) {
  if(sv.name !== undefined) {
    return sv.name;
  } else {
    return "Unexplored star " + sv.id;
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

function inRange(a,b) {
  return dist(a,b) < mapView.playerInfo.range;
}

function etaHrs(a, b) {
  return (dist(a, b)/mapView.playerInfo.speed*
    (24.0/mapView.yearsPerDay)).toFixed(1);
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
    var startPs = myCurPos(e);
    var startPx = startPs[0];
    var startPy = startPs[1];
    
    if(startAction !== undefined) { 
      startAction(startPx, startPy); 
    }
    
    var dragged = false;
    
    var moveHandle = function(moveEvent) {
      var ps = myCurPos(moveEvent);
      var px = ps[0]; var py = ps[1];
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
        var endPs = myCurPos(upEvent);
        var endPx = endPs[0]; var endPy = endPs[1]; 
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

function drawLineFromEntityToObj(ctx, style, fromEnt, toObj) {
  ctx.strokeStyle = style;
  
  ctx.beginPath();
  ctx.moveTo(fromEnt.px, fromEnt.py);
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
  ctx.font ='bold 12px Arial';
  
  // clear map entites
  mapPort.entities = [];
  
  // draw background
  ctx.fillStyle = 'rgb(0,0,0)';
  ctx.fillRect(0,0,w,h);
  
  function inViewPort(itemWithPosition) {
    var ps = spaceToPixel(itemWithPosition.x, itemWithPosition.y);
    var px = ps[0]; var py = ps[1]; 
    return px < canvas.width && px > 0 &&
           py < canvas.height && py > 0;
  }
  
  function makeEntity(px, py, xtol, ytol, type, obj) {
    return {
      euid: obj.euid,
      plft: px-xtol, 
      prht: px+xtol,
      ptop: py-ytol, 
      pbot: py+ytol,
      px: px,
      py: py,
      type: type,
      obj: obj
    };
  }
  
  function drawFleetView(fv) {
    var ps = spaceToPixel(fv.x, fv.y);
    var px = ps[0]; var py = ps[1]; 
    
    ctx.save();
    
    ctx.translate(px, py);
    
    if(!fv.moving) {
      px+=20;
      py-=15;
      ctx.translate(20, -15);
    } else {
      
      var toStarView = mapView.starViews[fv.toStarId];
      var fromStarView = mapView.starViews[fv.fromStarId];
      
      // moving, rotate ship accordingly...
      if(toStarView.x < fromStarView.x) {
        ctx.rotate(Math.PI);
      }
    }
    
    ctx.beginPath();
    ctx.fillStyle = playerIdToColor[fv.playerId];
    ctx.moveTo(-6, -6);
    ctx.lineTo(10, 0);
    ctx.lineTo(-6, 6);
    ctx.lineTo(-3, 0);
    ctx.lineTo(-6, -6);
    ctx.closePath();
    ctx.fill();
    
    ctx.restore();
    
    if(fv.moving) {
      drawLineInSpace(ctx, 'rgba(255,255,255,0.8)', fv, toStarView);
    }
    
    mapPort.entities.push(
      makeEntity(px, py, 12, 8, 'fv', fv));
  }
  
  function drawStarView(sv) {
    var ps = spaceToPixel(sv.x, sv.y);
    var px = ps[0]; var py = ps[1];
    
    ctx.beginPath();
    ctx.fillStyle = classToFillStyle[sv.sClass];
    ctx.arc(px, py, 10.0, 0, 2*Math.PI, true);
    ctx.fill();
    
    if(sv.name) {
      var textC = null;
      if(sv.knownOwnerId !== undefined) {
        textC = playerIdToColor[sv.knownOwnerId];
      } else {
        textC = '#888888';
      }
      ctx.fillStyle = textC;
      ctx.fillText(sv.name, px-10, py+25);
    }
    
    mapPort.entities.push(
      makeEntity(px, py, 14, 14, 'sv', sv));
    
    if(sv.visibleGarrison) {
      drawFleetView(sv.visibleGarrison);
    }
  }
  
  // draw all qualifying stars and fleets
  var visibleStarViews = mapView.starViews.filter(inViewPort); 
  visibleStarViews.map(drawStarView);
  mapView.movingFleetViews.filter(inViewPort).map(drawFleetView);
  
  // draw selected box  
  var e = mapPort.selectedEntity();
  if(e !== undefined) {
    ctx.fillStyle = "rgba(255,255,255,0.3)";
    ctx.fillRect(e.plft, e.ptop, e.prht-e.plft, e.pbot-e.ptop);
    ctx.strokeStyle = "rgba(255,255,255,1.0)";
    ctx.strokeRect(e.plft, e.ptop, e.prht-e.plft, e.pbot-e.ptop);
    
    if(e.type == 'fv') {
      var greenStyle = "rgba(128,255,64, 0.8)";
      var redStyle   = "rgba(255,64,0, 0.8)";
      
      // if stationary fleet selected, draw possible paths
      if(!e.obj.moving) {
        var inRangeStarViews = mapView.starViews.filter(function(sv) { 
          return inRange(e.obj, sv); });
        
        inRangeStarViews.map(function(sv) {
          drawLineInSpace(ctx, "rgba(128,255,64, 0.2)", e.obj, sv);
        });
        
        if(fleetDispatchToStarId !== null) {
          var toStar = mapView.starViews[fleetDispatchToStarId];
          if(inRange(e.obj, toStar)) {
            var lineStyle = greenStyle;
          } else {
            var lineStyle = redStyle;
          }
          
          drawLineFromEntityToObj(ctx, lineStyle, e, toStar); 
        }
      } else {
        // for a moving selected fleet, make path green
        var toStar = mapView.starViews[e.obj.toStarId];
        drawLineFromEntityToObj(ctx, greenStyle, e, toStar);
      }
    }
  }
}
