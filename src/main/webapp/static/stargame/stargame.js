function setupSliders() {
  nSliders = $('.research-slider').length;
  
  $('.research-slider').slider({
    range: "min",
    min: 0,
    max: 1000,
    slide: function(event, ui) {
      oldValue = $(event.target).slider("option", "value");
      newValue = ui.value;
      
      delta = newValue-oldValue;
      
      $('.research-slider').each(function(i, elem) {
        if(elem != ui.handle) {
          elemOldValue = $(elem).slider("option", "value");
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
  sliders = $('.research-slider');
  for(i = 0; i < sliders.length; i++) {
    $(sliders[i]).slider("option", "value", allocs[i]*1000.0);
  }
}

$(document).ready(function() {
  
});
