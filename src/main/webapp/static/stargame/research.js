function setupSliders(slideStopF) {
  var nSliders = $('.research-slider').length;
  
  $('.research-slider').slider({
    range: "min",
    min: 1,
    max: 1000,
    slide: function(event, ui) {
      var oldValue = $(event.target).slider("option", "value");
      var newValue = ui.value;
      
      var oldLeftovers = 1000-oldValue;
      var leftovers = 1000-newValue;
      
      $('.research-slider').each(function(i, elem) {
        if(elem != ui.handle) {
          var elemOldValue = $(elem).slider("option", "value");
          $(elem).slider("option", "value", 
            (elemOldValue+1)/(oldLeftovers+5)*leftovers);
        }
      });
    },
    stop: function(event, ui) { 
      slideStopF(); 
    }
  });
};

function getSliderVals() {
  var sliderVals = $.map($('.research-slider'), function(elem) { 
    return $(elem).slider("option", "value"); 
  }); 
  return sliderVals; 
};

function setSliderVals(allocs) {
  var i = 0;
  var sliders = $('.research-slider');
  for(i = 0; i < sliders.length; i++) {
    $(sliders[i]).slider("option", "value", allocs[i]*1000.0);
  }
}
