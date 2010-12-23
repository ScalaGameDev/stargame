var nDesignsInFleet = 0;
var fleetDispatchQuantities = [0,0,0,0,0,0];
function showFleetSidebar(fv) {
  if(fv.moving) {
    var sv = mapView.starViews[fv.toStarId];
    $('#fleet-name').html("Fleet in transit to " + starViewName(sv));
  } else {
    var sv = mapView.starViews[fv.fromStarId];
    $('#fleet-name').html("Fleet at " + starViewName(sv));
  }
  
  var designs = playerInfo.player.designs;
  
  // i = design number
  nDesignsInFleet = 0;
  for(var i = 0; i < fv.ships.length; i++) {
    if(fv.ships[i] > 0) {
      var fleetQId = "fleetQuantity-"+i.toString(); 
      $('#fleetContent-'+nDesignsInFleet.toString()).html(
        "<strong>"+ designs[i].name +"</strong><br/>"+
        "<span id='"+fleetQId+"Num'>"+ fv.ships[i].toString() +"</span>" +
        "<div id='"+fleetQId+"' class='fleet-slider' />");
      
      if(!fv.moving) {
        // necessary to defeat closures + shitty scoping
        function adjustSlider(currentFleetQId, currentI) {
          return function(event, ui) {
            $('#'+currentFleetQId+'Num').html(ui.value.toString());
            fleetDispatchQuantities[currentI] = ui.value; 
          };
        }
        
        $('#'+fleetQId).slider({
          range: "min",
          min: 0,
          max: fv.ships[i],
          slide: adjustSlider(fleetQId, i),
          value: fv.ships[i]
        });
      }
      nDesignsInFleet += 1;
    }
  }
  
  fleetDispatchQuantities = fv.ships;
  
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
      jsonDispatchFleet(selected.obj.uuid, fleetDispatchQuantities, 
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
    // make sure that mouse point does not move on zoom...
    var zoomToSpacePt = pixelToSpace.apply(this, curPos(e, $('#map-canvas')));
    
    mapPort.doNormalizedZoom(d>0, zoomToSpacePt[0], zoomToSpacePt[1]);
    drawMap();
    return false;
  });
  
  $('#map-canvas').click(function(event,d) {

  });
  
});
