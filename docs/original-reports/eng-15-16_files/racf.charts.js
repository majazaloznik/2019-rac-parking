var RACF = window.RACF || {};

RACF.Charts = (function ($) {

	function init() {
		$('.chart-set').each(function () {
			initiateChartCarousel(this);
		});

		$('.chart-set.active')
			.on('click', '.next', function () {
				var newLeft,
					jqThis = $(this),
					strip = jqThis.siblings('.strip'),
					activeChart = strip.find('.active'),
						moveDistance = activeChart.outerWidth();

				if (!strip.hasClass('animating')) {
					strip.addClass('animating');
					var newLeft = '-=' + moveDistance;

					if (activeChart.is(':last-child')) {
						newLeft = '';
					}

					strip.animate({
						left: newLeft
					}, 500, function () {
						var nextChart;
						if (activeChart.is(':last-child')) {
							nextChart = strip.find('.chart-embed:first-child');
						}
						else {
							nextChart = activeChart.next();
						}
						activeChart.removeClass('active');
						nextChart.addClass('active');
						strip.removeClass('animating');
					});
				}
			})
			.on('click', '.prev', function () {
				var newLeft,
					jqThis = $(this),
					strip = jqThis.siblings('.strip'),
					activeChart = strip.find('.active'),
						moveDistance = activeChart.outerWidth();

				if (!strip.hasClass('animating')) {
					strip.addClass('animating');
					newLeft = '+=' + moveDistance;

					if (activeChart.is(':first-child')) {
						newLeft = '-' + ((strip.find('.chart-embed').length - 1) * moveDistance);
					}

					strip.animate({
						left: newLeft
					}, 500, function () {
						var prevChart;
						if (activeChart.is(':first-child')) {
							prevChart = strip.find('.chart-embed:last-child');
						}
						else {
							prevChart = activeChart.prev();
						}
						activeChart.removeClass('active');
						prevChart.addClass('active');
						strip.removeClass('animating');
					});
				}
			});
	}

	function initiateChartCarousel(setElm) {
		var set = $(setElm),
			strip = set.find('.strip'),
			charts = strip.find('.chart-embed'),
			chartCount = charts.length,
			stripWidth = 100 * chartCount,
			chartWidth = 100 / chartCount,
			chartPadding = 4 / chartCount;

		strip.css({ width: stripWidth + '%' });
		charts.css({ width: chartWidth + '%', padding: '0 ' + chartPadding + '%' });

		if (chartCount > 1) {
			set.addClass('active');
		}
	}

	return {
		init: init
	}

}(jQuery));

jQuery(RACF.Charts.init);