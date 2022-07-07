$(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'printButton') {
      window.print();
    }
});
