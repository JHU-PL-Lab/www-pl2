$(document).ready(function () {
    var HeightDiv = $("div").height();
    var HeightTable = $("table.date").height();
    if (HeightTable > HeightDiv) {
        var FontSizeTable = parseInt($("table.date").css("font-size"), 10);
        while (HeightTable > HeightDiv && FontSizeTable > 5) {
            FontSizeTable--;
            $("table.date").css("font-size", FontSizeTable);
            HeightTable = $("table.date").height();
        }
    }
});
