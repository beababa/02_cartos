(function ($) {
	"use strict";
	$.fn.isInViewport = function () {
		var elementTop = $(this).offset().top;
		var elementBottom = elementTop + $(this).outerHeight();

		var viewportTop = $(window).scrollTop();
		var viewportBottom = viewportTop + $(window).height();

		return elementBottom > viewportTop && elementTop < viewportBottom;
	};

	// =======Sticky-header========>>>>>
	$(window).on("scroll", function () {
		var scroll = $(window).scrollTop();
		if (scroll < 90) {
			$(".sticky-navbar").removeClass("sticky");
		} else {
			$(".sticky-navbar").addClass("sticky");
		}
	});

	// Exécuter une fois au chargement pour définir l'état initial
	// (utile quand la balise header a déjà la classe 'sticky' par défaut)
	$(window).trigger("scroll");
	// =======Sticky-header========>>>>>

	// =======Offcanvas-data-bs-dismiss="offcanvas"========>>>>>
	if ($("#logisticsNavbar-2").length > 0) {
		const navContentmenu = new bootstrap.Offcanvas("#logisticsNavbar-2");
		$(document).on("click", ".nav-link", function () {
			navContentmenu.hide();
		});
	}
	// =======Offcanvas-data-bs-dismiss="offcanvas"========>>>>>

	// =======Swiper .testimonialSwiper-2========>>>>>
	if ($(".testimonialSwiper-2").length > 0) {
		new Swiper(".testimonialSwiper-2", {
			loop: true,
			spaceBetween: 30,
			speed: 2000,
			navigation: {
				nextEl: ".testimonialSwiper-2-button-next",
				prevEl: ".testimonialSwiper-2-button-prev",
			},
		});
	}
	// =======Swiper .testimonialSwiper-2========>>>>>

	new WOW().init();

	// ======= Hover Element  ========>>>>>
	document.addEventListener("DOMContentLoaded", function () {
		const sections = document.querySelectorAll(".hover-element");

		sections.forEach((section) => {
			const cards = section.querySelectorAll(".hover-item");
			const defaultActiveCard = section.querySelector(".hover-item.active");

			let activeCard = defaultActiveCard;

			cards.forEach((card) => {
				card.addEventListener("mouseover", function () {
					if (activeCard) {
						activeCard.classList.remove("active");
					}
					this.classList.add("active");
					activeCard = this;
				});
			});

			section.addEventListener("mouseleave", function () {
				if (activeCard) {
					activeCard.classList.remove("active");
				}
				if (defaultActiveCard) {
					defaultActiveCard.classList.add("active");
					activeCard = defaultActiveCard;
				}
			});
		});
	});
	// ======= Hover Element  ========>>>>>
})(jQuery);

// =================  Dropdown on Hover =============
document.addEventListener("DOMContentLoaded", function () {
	if (window.innerWidth > 992) {
		document.querySelectorAll(".hover-menu .nav-item.dropdown").forEach(function (everyitem) {
			everyitem.addEventListener(
				"mouseover",
				function (e) {
					let el_link = this.querySelector("a[data-bs-toggle]");
					if (el_link !== null) {
						let nextEl = el_link.nextElementSibling;
						el_link.classList.add("show");
						if (nextEl !== null && this.contains(nextEl)) {
							nextEl.classList.add("show");
						}
					}
				}.bind(everyitem)
			);
			everyitem.addEventListener(
				"mouseleave",
				function (e) {
					let el_link = this.querySelector("a[data-bs-toggle]");
					if (el_link !== null) {
						let nextEl = el_link.nextElementSibling;
						if (nextEl !== null && this.contains(nextEl)) {
							el_link.classList.remove("show");
							nextEl.classList.remove("show");
						}
					}
				}.bind(everyitem)
			);
		});
	}
	// end if innerWidth
});
