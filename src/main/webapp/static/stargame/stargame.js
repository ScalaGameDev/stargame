var fleetDispatchQuantity = 0;
var fleetDispatchToStarId = null;
var lastResearchCategory = 0;

function showFleetSidebar(fv) {
  $('#fleetContent').html(
    "Ship count<br/>" +
    "<span id='fleetNum'>"+ fleetDispatchQuantity.toString() +"</span>" +
    "<div id='fleetSlider' class='fleet-slider' />");
  
  if(fv.moving) {
    var sv = mapView.starViews[fv.toStarId];
    $('#fleet-name').html("Fleet in transit to " + starViewName(sv));
    $('#fleetDispatch').html("ETA: " + etaHrs(fv, sv) + "hrs");
  } else {
    var sv = mapView.starViews[fv.fromStarId];
    $('#fleet-name').html("Fleet at " + starViewName(sv));
    
    $('#fleetSlider').slider({
      range: "min",
      min: 0,
      max: fv.ships,
      slide: function(event, ui) {
        $('#fleetNum').html(ui.value.toString());
        fleetDispatchQuantity = ui.value;
        drawMap();
      },
      value: fleetDispatchQuantity
    });
    
    if(fleetDispatchToStarId !== null) {
      var toStar = mapView.starViews[fleetDispatchToStarId];
      
      var header = "Dispatch to " + starViewName(toStar) + "<br/>" +
        "Distance: " + dist(fv, toStar).toFixed(2) + "<br/>";
      
      if(!inRange(fv, toStar)) {
        $('#fleetDispatch').html(header +
          "Destination out of range. Research longer range.");
      } else {
        $('#fleetDispatch').html(header +
          "ETA: " + etaHrs(fv, toStar) + "hrs" + 
          "<br/>" + 
          "<button id='doDispatchBtn'>Dispatch</button>"
        );
        
        $("#doDispatchBtn").button().click(function() {
           jsonDispatchShips(fv.fromStarId, fleetDispatchQuantity, 
             fleetDispatchToStarId);
        });
      }
    } else {
      $('#fleetDispatch').html("");
    }
  }
  
  selectSidebar("fleetinfo");
}

function showStarSidebar(sv) {
  $('#star-name').html(starViewName(sv));
  $('#star-class').html(sv.sClass);
  
  function row(tag, items) {
    return "<tr>" + items.map(function(item) { 
      return "<"+tag+">"+item+"</"+tag+">" }).join("") + "</tr>";
  }
  
  if(sv.planets !== undefined) {
    var planetsTable = 
      "<table id='planets-table'>" +
      row("th", ["Type", "Pop", "Income", ""]) +
      sv.planets.map(function(p) {
        p.maxPop = p.baseMaxPop*mapView.playerInfo.maxPopMultiplier;
        return row("td", [p.pType, 
          p.pop.toFixed(1) + " M",
          (p.pop*p.mineralWealth).toFixed(1) + " RU/yr",
          "<button id='BtnPlanet"+p.id+"'>Detail</button>"])
      }).join("") +
      "</table>";
    
    $('#star-planets').html(planetsTable);
    
    // Set up dialogs. Must be done after html set
    sv.planets.map(function(p) {
      function twoElemRow(a, b) {
        return "<tr><td><b>"+a+"</b></td><td>"+b+"</td></tr>\n";
      }
      $('#BtnPlanet'+p.id).click(function(e) {
        var diagContents = "<table>" +
          twoElemRow("Type", p.pType) +
          twoElemRow("Maximum Pop.", p.maxPop.toFixed(2) + " M") +
          twoElemRow("Pop. growth rate", p.popGrowthRate.toFixed(2) + " M/yr") +
          twoElemRow("Population", p.pop.toFixed(2) + " M") +
          twoElemRow("Mineral wealth", 
            p.mineralWealth.toFixed(2) + " RU/(M*yr)") +
          twoElemRow("Income rate", 
            (p.pop*p.mineralWealth).toFixed(2) + " RU/yr") +
          "</table>";
        var diagDiv = $('<div></div>').html(diagContents).dialog({
          title: sv.name + " " + p.id + " Details (Year " + 
                 mapView.gameYear.toFixed(1) + ")" 
        });
        diagDiv.bind('clickoutside', function() { 
          diagDiv.dialog('close');
        });
        return false;
      });
    });
  } else {
    $('#star-planets').html("");
  }
  
  selectSidebar("starinfo");
}

function selectEntities(entities) {
  // on select new entity, erase toStarId
  fleetDispatchToStarId = null;
  
  var entityIndex = $.inArray(mapPort.selectedEntity(), entities);
  
  // if already selecting stuff, choose next item
  if(entityIndex === -1) {
    var clickIndex = 0;
  } else {
    var clickIndex = (entityIndex+1)%entities.length;
  }
  
  var e = entities[clickIndex];
  mapPort.selectedEuid = e.euid;
  
  if(e.type === "fv") {  
    // select fleet again: reset ship count to max
    fleetDispatchQuantity = e.obj.ships;
  }
  
  refreshEntitySelection(); // show the sidebars
  
  return false;
}

function refreshEntitySelection() {
  var e = mapPort.selectedEntity();
  if(e !== undefined) {
    if(e.type === 'sv') {
      showStarSidebar(e.obj);
    } else if(e.type === 'fv') {
      // don't reset fleet dispatch to or number
      showFleetSidebar(e.obj);
    }
  }
}

function clickEntities(entities) {
  var selected = mapPort.selectedEntity();
  
  if(selected !== undefined && selected.type === 'fv' && !selected.obj.moving) {
    var clickedStars = entities.filter(function(e) { 
      return e.type === 'sv'; 
    });
    
    if(clickedStars.length > 0) {
      fleetDispatchToStarId = clickedStars[0].obj.id;
      
      if(fleetDispatchToStarId === selected.obj.fromStarId) {
        selectEntities(entities);
      } else {
        showFleetSidebar(selected.obj);
      }
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
