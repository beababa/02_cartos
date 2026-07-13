/**
 * Gestion des erreurs de l'api
 */
function handle_ajax_error(textStatus, xhr, div_errors, div_errors_content) {
	$("#" + div_errors).fadeIn(300);
	//.delay(4000)
	//	.fadeOut();
	var erreurs = "";
	if (textStatus === "timeout") {
		erreurs = "Veuillez vérifier votre connexion internet.";
	} else if (xhr.readyState == "0") {
		erreurs = "Le serveur est injoignable. Veuillez réessayer dans quelques secondes.";
		if (xhr.statusText != undefined && xhr.statusText != "") {
			erreurs = erreurs + "<br />" + xhr.statusText;
		}
	} else if (xhr.status == 403 || xhr.status == 401) {
		// message par défaut
		erreurs = "Vous n'êtes pas autorisé à effectuer cette opération";
		// va chercher le message spécifique si il existe
		try {
			var json = JSON.parse(xhr.responseText);
			if (json != undefined) {
				erreurs = json.message;
			}
		} catch (e) {}
	} else if (xhr.status == 400 || xhr.status == 404 || xhr.status == 406) {
		try {
			var json = JSON.parse(xhr.responseText);
			if (json.message != undefined && json.errors == undefined) {
				erreurs = erreurs + json.message + "<br />";
			} else {
				$.each(json.errors, function (index, value) {
					erreurs = erreurs + value.defaultMessage + "<br />";
				});
			}
		} catch (e) {}
	} else if (xhr.status != 200) {
		erreurs = xhr.readyState + " - " + xhr.status + " - " + xhr.responseText;
	}
	afficher_erreur("Erreur", erreurs);
}

function isArray(what) {
	return Object.prototype.toString.call(what) === "[object Array]";
}

/**
 * Menus
 */

function getUrlParameter(sParam) {
	var sPageURL = window.location.search.substring(1),
		sURLVariables = sPageURL.split("&"),
		sParameterName,
		i;

	for (i = 0; i < sURLVariables.length; i++) {
		sParameterName = sURLVariables[i].split("=");

		if (sParameterName[0] === sParam) {
			return sParameterName[1] === undefined ? true : decodeURIComponent(sParameterName[1]);
		}
	}
	return false;
}

function afficher_erreur(titre, message) {
	$().simpleModal({
		name: "example",
		title: "<div style='color:#BC1519'><i class='fa-solid fa-triangle-exclamation' style='margin-right: 8px;'></i>" + titre + "</div>",
		content: message,
		size: "middle",
		freeze: true,
		callback: function () {
			$(".simple-modal--example .my-button").click(function () {
				console.log("Click from modal");
			});
		},
	});
}

function capitalizeFirstLetter(string) {
	return string.charAt(0).toUpperCase() + string.slice(1);
}

// chargement des bannières des courses en lazy loading
$(function () {
	const observer = new IntersectionObserver(function (entries, obs) {
		entries.forEach((entry) => {
			if (entry.isIntersecting) {
				const $div = $(entry.target);
				const bg = $div.attr("data-bg");
				if (bg) {
					// Charger l'image de façon asynchrone
					const img = new Image();
					img.onload = function () {
						$div.css("background-image", "url(" + bg + ")");
						$div.hide().fadeIn(500); // petit effet si tu veux
					};
					img.src = bg;
					obs.unobserve(entry.target); // ne plus observer après le chargement
				}
			}
		});
	});

	$(".lazy-bg").each(function () {
		observer.observe(this);
	});

	// Crée un observateur qui regarde quand une image entre dans le viewport
	const observer2 = new IntersectionObserver(function (entries, obs) {
		console.log("");
		entries.forEach((entry) => {
			if (entry.isIntersecting) {
				const $img = $(entry.target);
				const src = $img.attr("data-src");
				if (src) {
					// Charger l'image de manière asynchrone
					const img = new Image();
					img.onload = function () {
						$img.attr("src", src).hide().fadeIn(300);
					};
					img.src = src;
					obs.unobserve(entry.target); // stoppe l'observation
				}
			}
		});
	});

	$(".lazy").each(function () {
		observer2.observe(this);
	});
});
// 2️⃣ Fonction pour construire la query string
function buildQueryString(params) {
	if (!params || params.length === 0) return "";

	const query = params.map((p) => `${encodeURIComponent(p.param)}=${encodeURIComponent(p.valeur)}`).join("&");

	return "?" + query;
}
function onLanguageChange(langCode) {
	console.log("Langue sélectionnée :", langCode);
	const url = new URL(window.location.href);

	// Récupérer le slug adapté à cette langue
	const slugs = window.PAGE_SLUGS || {};
	const newSlug = slugs[langCode];

	console.log("newSlug :>> ", newSlug);

	// Si on n'a pas de slug traduit, on garde l'actuel
	const segments = url.pathname.split("/");
	const currentSlug = segments[2] || "";

	url.pathname = "/" + langCode + "/" + (newSlug || currentSlug);

	// (optionnel) enlever le param ?lang=...
	url.searchParams.delete("lang");

	window.location.href = url.toString();
}

document.addEventListener("DOMContentLoaded", function () {
	// Récupérer tous les sélecteurs de langue (pas seulement le premier)
	const selectors = document.querySelectorAll(".lang-selector");

	selectors.forEach((selector) => {
		const toggle = selector.querySelector(".lang-selector__toggle");
		const options = selector.querySelectorAll(".lang-selector__option");
		const labelEl = selector.querySelector(".lang-selector__label");
		const flagEl = selector.querySelector(".lang-selector__flag");

		toggle.addEventListener("click", function (e) {
			e.stopPropagation();
			const isOpen = selector.classList.toggle("lang-selector--open");
			toggle.setAttribute("aria-expanded", isOpen ? "true" : "false");
		});

		options.forEach((option) => {
			option.addEventListener("click", function (e) {
				e.stopPropagation();

				const lang = option.dataset.lang;
				const label = option.dataset.label;
				const flag = option.querySelector(".lang-selector__flag").textContent;

				// Mettre à jour tous les sélecteurs de langue
				document.querySelectorAll(".lang-selector__option").forEach((opt) => {
					opt.classList.remove("is-active");
					opt.setAttribute("aria-selected", "false");
				});

				// Ajouter la classe active à l'option cliquée
				option.classList.add("is-active");
				option.setAttribute("aria-selected", "true");

				// Mettre à jour l'affichage de tous les sélecteurs
				document.querySelectorAll(".lang-selector__label").forEach((lbl) => {
					lbl.textContent = label;
				});
				document.querySelectorAll(".lang-selector__flag").forEach((flg) => {
					flg.innerHTML = flag;
				});

				// Fermer tous les sélecteurs
				document.querySelectorAll(".lang-selector").forEach((sel) => {
					sel.classList.remove("lang-selector--open");
					sel.querySelector(".lang-selector__toggle").setAttribute("aria-expanded", "false");
				});

				onLanguageChange(lang);
			});
		});

		document.addEventListener("click", function (e) {
			// Ne fermer que si le clic n'est pas sur un sélecteur de langue
			if (!e.target.closest(".lang-selector")) {
				selector.classList.remove("lang-selector--open");
				toggle.setAttribute("aria-expanded", "false");
			}
		});

		document.addEventListener("keydown", function (e) {
			if (e.key === "Escape") {
				selector.classList.remove("lang-selector--open");
				toggle.setAttribute("aria-expanded", "false");
			}
		});
	});
});

document.addEventListener("DOMContentLoaded", function () {
	const sticky = document.getElementById("responsive-sticky-home");
	if (!sticky) return;

	const isResponsive = () => window.innerWidth <= 768; // seuil "responsive" (à adapter)

	function toggleSticky() {
		// si pas responsive, on cache toujours
		if (!isResponsive()) {
			sticky.classList.remove("visible");
			return;
		}

		const scrollTop = window.scrollY || document.documentElement.scrollTop;
		const docHeight = document.documentElement.scrollHeight - window.innerHeight;
		const scrollPercent = (scrollTop / docHeight) * 100;

		// Afficher à partir de 10 %
		sticky.classList.toggle("visible", scrollPercent >= 10);
	}

	// Au scroll et au resize (pour adapter si on tourne l’écran ou change de taille)
	window.addEventListener("scroll", toggleSticky);
	window.addEventListener("resize", toggleSticky);

	// Lancer une première fois au chargement
	toggleSticky();
});
