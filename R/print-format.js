
$(document).ready(function () {
var printButton = $('<span class="printButton">Print</span>');
      printButton.on('click', function() {
        window.print();
      });
      topicsFooter.append(printButton);
      topicsList.append(topicsFooter);
      return topicsList;
}
