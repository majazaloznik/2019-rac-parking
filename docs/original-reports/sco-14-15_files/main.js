var RACF = window.RACF || {};

RACF.FrontEndUI = (function racfFrontEndUISetup($)
{

	function init()
	{
		setVideo();
		setTab();
		imageSliderGallery();
		setNav();

		setupSearchUi();
		boxesHeight();
		tweetie();
		iframeLoader();
	};

	$(window).resize(function () {
		boxesHeight();
	});

	function boxesHeight() {
		var boxes = $(".box-items >div"),
			count = boxes.length,
			headers = boxes.find("h3"),
			defaultHeight = 0;

		headers.height("auto");

		boxes.each(function () {
			var $this = $(this),
				$thisHeader = $this.find("h3"),
				$thisHeaderHeight = $thisHeader.height();

			if ($thisHeaderHeight > defaultHeight) {
				defaultHeight = $thisHeaderHeight;
			}
			if (!--count) setBoxesHeight(headers,defaultHeight);
		})
	}
	function setBoxesHeight(headers, defaultHeight) {

		headers.height(defaultHeight);
	}

	function setVideo()
	{
		var $pageoverlay = $('.page-overlay');
		$iframe = $pageoverlay.find('iframe');
		$('.videos .fn-player').click(function ()
		{
			$pageoverlay.show();
			$iframe.attr('src', '//www.youtube.com/embed/' + $(this).attr('id') + '?rel=0&autoplay=1&wmode=transparent');
			//  $iframe.attr('width', $(this).attr('data-width'));
			//  $iframe.attr('height', $(this).attr('data-height'));
		});
		$('.page-overlay .close').click(function ()
		{
			$pageoverlay.hide();
			$iframe.attr('src', '');
		});

		$('.flip-gallery .filter').removeClass('open');
		$('.flip-gallery').removeAttr('style');
	}

	function setTab()
	{
		var tabular = $('#tabular'),
			activeItem = tabular.find('.titles .active');

		if (activeItem.length > 0)
		{
			tabular.find('ul').eq(activeItem.index() - 1).show().siblings('ul').hide();
		}

		$('.temp-press #tabular .titles > span').click(function ()
		{
			var jqThis = $(this),
				key = jqThis.data('month'),
				tabular = $('#tabular');

			jqThis.addClass('active').siblings().removeClass('active');
			tabular.find('ul[data-month=' + key + ']').show()
				.siblings('ul').hide();

			if (key === -1)
			{
				tabular.addClass('with-archive');
				$('.archive-years').show()
					.find('span').first().trigger('click');
			}
			else
			{
				tabular.removeClass('with-archive');
				$('.archive-years').hide();
			}
		});

		tabular.find('.archive-years span[data-year]').on('click', function (e)
		{
			var jqThis = $(this),
				year = jqThis.data('year'),
				archiveList = $('#tabular ul[data-month="-1"]');

			jqThis.addClass('active').siblings('span').removeClass('active');
			archiveList.children().hide();
			archiveList.find('li[data-year=' + year + ']').show();
		});

		$('.temp-press #tabular .titles > span').first().trigger('click');//.addClass('active')
	}

	function imageSliderGallery()
	{
		$('.image-slider > .slide').cycle({
			slideResize: 0,
			containerResize: 0,
			pause: 1,
			pauseOnPagerHover: 1,
			next: '.image-slider .ico-arrow-lined.rt',
			prev: '.image-slider .ico-arrow-lined.lt',
			pager: '.image-slider > .nav',
			pagerAnchorBuilder: function (idx, slide)
			{
				return '<span>&#149;</span>';
			}
		});

		if ($('.slide').children().length == 1)
		{
			$('.image-slider .ico-arrow-lined').hide();
		}

		$('.image-slider .ico-arrow-lined ').click(function ()
		{
			$('.image-slider > .slide').cycle('pause');
		});
	}

	function setNav()
	{
		$('.menu-handle').on('click', function ()
		{
			$(this).closest('nav').find('ul').toggle();
		});

		$('.sitemap > div > span.parent').on('click', function (e)
		{
			if ($(e.target).is('span')) // It has to be, based on the selector?
			{
				// Instead of toggling the next element here, why not use CSS?
				$(this).toggleClass('active').next().toggle();
			}
		});

		var sharethisLoad = setInterval(function ()
		{
			if ($('.stLarge').length >  0)
			{
				$('.social-share-dropdown [class^="st_"], .social-share-dropdown [class*=" st_"]').each(function ()
				{
					$(this).find('.stLarge').text($(this).attr('displaytext'));
					//alert($(this).attr('displaytext'));
				});
				clearInterval(sharethisLoad);
				setTimeout(function () { $('.social-share-dropdown').css('visibility', 'visible'); }, 2000);
				//alert('loaded');
			}
		}, 500);

		$('.social-share-dropdown .btn_share').on('click', function ()
		{
			$(this).parent('.social-share-dropdown').toggleClass('open');
		});
		
		
	}

	function setupSearchUi()
	{
		var searchForm = $('#search-form'),
			icos = $("header .icon-twitter, header .icon-wordpress, header .icon-envelope"),
			showSearchButton = $('.icon-RACF-search_icon-V4');

		$(document).click(function (e) {
			
			if (!$(e.target).parent().hasClass('social-links') && $(e.target).parent().attr('id') != 'search-form') {
				searchForm.removeClass('show');
				setTimeout(function () { icos.removeClass('hide'); }, 600);
			}

		})

		showSearchButton.on('click', function (e)
		{
			$('#searchtext').focus();
			searchForm.toggleClass('show');
			if (searchForm.hasClass('show'))
			{
				icos.addClass('hide');
			}
			else
			{
				
				setTimeout(function () { icos.removeClass('hide'); }, 600);
			}
			//setTimeout(function () { $("header .icon-twitter, header .icon-wordpress, header .icon-envelope").toggleClass('hide'); }, 600);
			
		});

		searchForm.on('submit', function (e)
		{
			var searchText = $(this).find('input[name=searchtext]');

			if (searchText && searchText.val() === '')
			{
				e.preventDefault();
			}
		});
		
	}
	
	function tweetie() {
		var twitterFeed = $('.twitter-feed');
		
		twitterFeed.twittie({
			'count': 4,
			'hideReplies': true,
			'template': '{{tweet}}',
			'apiPath': '/wp-content/themes/racfoundation/js/tweetie/api/tweet.php',
		});
	}
	
	function iframeLoader() {
		$('iframe').load(function() {
    		$(this).css('background', 'none');
		});
	}

	return {
		init: init
	};

}(jQuery));

jQuery(RACF.FrontEndUI.init);