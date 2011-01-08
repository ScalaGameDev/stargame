var fleetDispatchQuantity = 0;

function showFleetSidebar(fv) {
  if(fv.moving) {
    var sv = mapView.starViews[fv.toStarId];
    $('#fleet-name').html("Fleet in transit to " + starViewName(sv));
  } else {
    var sv = mapView.starViews[fv.fromStarId];
    $('#fleet-name').html("Fleet at " + starViewName(sv));
  }
      
  $('#fleetContent').html(
    "Ship count<br/>" +
    "<span id='fleetNum'>"+ fv.ships.toString() +"</span>" +
    "<div id='fleetSlider' class='fleet-slider' />");
  
  if(!fv.moving) {
    $('#fleetSlider').slider({
      range: "min",
      min: 0,
      max: fv.ships,
      slide: function(event, ui) {
        $('#fleetNum').html(ui.value.toString());
        fleetDispatchQuantity = ui.value;
        drawMap();
      },
      value: fv.ships
    });
  }
  
  selectSidebar("fleetinfo");
}

function selectEntities(entities) {
  var entityIndex = $.inArray(mapPort.selectedEntity(), entities);
  
  // if already selecting stuff, choose next item
  if(entityIndex === -1) {
    var clickIndex = 0;
  } else {
    var clickIndex = (entityIndex+1)%entities.length;
  }
  
  var e = entities[clickIndex];
  mapPort.selectedEuid = e.euid;
  
  if(e.type === "sv") {
    $('#star-name').html(starViewName(e.obj));
    $('#star-class').html(e.obj.sClass);
    selectSidebar("starinfo");
  } else if(e.type === "fv") {
    showFleetSidebar(e.obj);
  }
  return false;
}

function clickEntities(entities) {
  var selected = mapPort.selectedEntity();
  
  if(selected !== undefined && selected.type === 'fv' && !selected.obj.moving) {
    var clickedStars = entities.filter(function(e) { 
      return e.type === 'sv'; 
    });
    
    if(clickedStars.length > 0) {
      var clickedStarId = clickedStars[0].obj.id;
      jsonDispatchShips(selected.obj.uuid, fleetDispatchQuantity, 
        clickedStarId);
    } else {
      // chose something else than a star
      selectEntities(entities);
    }
  } else {
    // no fleet currently selected
    selectEntities(entities);
  }
  
  return false;
}

function takeHint(euid) {
  mapPort.selectedEuid = euid;
  var entity = mapPort.selectedEntity();
  if(entity !== undefined) {
    selectEntities([entity]);
  }
  drawMap();
}

$(document).ready(function() {
  var dragStartX = 0, dragStartY = 0;
  setDragAction($('#map-canvas'), function(px,py) {
      
    mapPort.x = mapPort.x + (dragStartX-px)/mapPort.PixelsPerLy;
    mapPort.y = mapPort.y + (dragStartY-py)/mapPort.PixelsPerLy;
    dragStartX = px;
    dragStartY = py;
    
    mapPort.normalizePos();
    
    drawMap();

    return false;
  },
  function(px,py) {
    dragStartX = px;
    dragStartY = py;
    return false;
  }, 
  function(px,py, dragged) {
    if(!dragged) {
      function clicked(ent) {
        return px > ent.plft && px < ent.prht && 
               py > ent.ptop && py < ent.pbot;
      }
      
      var clickedEntities = mapPort.entities.filter(clicked);
      
      if(clickedEntities.length > 0) {
        clickEntities(clickedEntities);
      } else {
        delete mapPort.selectedEuid;
      }
      
      drawMap();
    }
    return false;
  });
  
  $('#map-canvas').wheel(function(e, d) {
    var zoomToSpacePt;
    var entity = mapPort.selectedEntity();
    if(entity !== undefined) {
      zoomToSpacePt = [entity.obj.x, entity.obj.y]; 
    } else {
      // make sure that mouse point does not move on zoom...
      zoomToSpacePt = pixelToSpace.apply(this, curPos(e, $('#map-canvas')));
    }
    
    mapPort.doNormalizedZoom(d>0, zoomToSpacePt[0], zoomToSpacePt[1]);
    drawMap();
    return false;
  });
  
});
