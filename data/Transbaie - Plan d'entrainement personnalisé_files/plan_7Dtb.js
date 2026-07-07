// Récupérer un plan d'entrainement
function afficher_plan_entrainement(json) {
	$("#recap-course").html(json.recap.recap_course);
	$("#recap-course2").html(json.recap.recap_course);
	$("#recap-objectif").html(json.recap.recap_objectif);
	$("#recap-niveau").html("Distance maximum : " + json.recap.recap_niveau_distance_max + " - Allure moyenne : " + json.recap.recap_niveau_allure_moyenne);
	$("#recap-seances").html(json.recap.recap_niveau_nb_seances_possibles + " - Journée(s) possible(s) : " + json.recap.recap_niveau_jours_possibles);
	$("#recap-sante").text(json.recap.recap_douleurs_problemes_sante || "");
}

function get_plan_entrainement(id) {
	$.ajax({
		type: "GET",
		contentType: "application/json",
		url: "/api/plan/" + id,
		dataType: "json",
		cache: false,
		async: false,
		timeout: 6000,
		crossDomain: true,

		success: function (json) {
			if (json == undefined) {
				afficher_erreur("Erreur", "Impossible de récupérer le plan d'entrainement");
			} else {
				afficher_plan_entrainement(json);
			}
		},
		error: function (xhr, textStatus, errorThrown) {
			handle_ajax_error(textStatus, xhr, "notification_warning", "div_errors_content");
		},
	}).always(function () {});
}

$(function () {
	var initialRating = 0;
	if ($("#avis-general-note").length > 0) {
		initialRating = $("#avis-general-note").html();
	}

	$("#lire-la-suite-finisher-line").on("click", function () {
		if ($(window).width() < 768) {
			$("#div-conversion-finisher-line").fadeIn(500);
		} else {
			$(".lien_conversion").trigger("click");
		}
		return false;
	});

	$("#avis-general").starRating({
		initialRating: initialRating,
		useFullStars: true,
		strokeColor: "#894A00",
		strokeWidth: 10,
		starSize: 40,
		hoverColor: "#f06728",
		activeColor: "#f06728",
		totalStars: 5,
		disableAfterRate: false,
		starShape: "rounded",
		starSize: 40,
		callback: function (currentRating, $el) {
			$("#avis-general-note").html(currentRating);
			enregistrer_avis(false);
		},
	});
	$("#avis-texte-envoyer").on("click", function () {
		enregistrer_avis(true);
	});

	// est ce qu'il faut afficher la div d'incitative à la conversion en mobile ?
	var plan_id = $("#plan-id").val();
	if (Cookies.get("conversion-plan-" + plan_id) == undefined && $(window).width() < 768) {
		setTimeout(function () {
			$("#div-conversion-finisher-line").fadeIn(500);
		}, 5000); // 5000 ms = 5 secondes
	}

	// fermer la div d'incitive à la conversion en mobile
	$("#div-conversion-finisher-line .fermer").on("click", function () {
		$("#div-conversion-finisher-line").remove();

		// enregistrement en cookie
		Cookies.set("conversion-plan-" + plan_id, 0, {
			expires: 7, // jours
		});
	});

	// sticky plan d'alimentation (mobile) : affichage 5s après l'arrivée, masquée 7 jours si fermée
	if ($("#div-nutrition-sticky").length) {
		if (Cookies.get("nutrition-plan-" + plan_id) == undefined && $(window).width() < 768) {
			setTimeout(function () {
				$("#div-nutrition-sticky").fadeIn(500);
			}, 5000); // 5000 ms = 5 secondes
		}

		$("#div-nutrition-sticky .ns-fermer").on("click", function () {
			$("#div-nutrition-sticky").remove();

			// enregistrement en cookie
			Cookies.set("nutrition-plan-" + plan_id, 0, {
				expires: 7, // jours
			});
		});
	}

	$(".lien_conversion").on("click", function () {
		var postData = {
			plan: $("#plan-id").val(),
			prix: $("#finish_line").val(),
		};
		$.ajax({
			type: "POST",
			url: "/api/plan/conversion",
			dataType: "json",
			cache: false,
			async: true,
			timeout: 6000,
			data: {
				params: JSON.stringify(postData),
			},
			crossDomain: true,
			success: function (json) {
				if (json == undefined) {
					$("#div_errors_content").html("Impossible de passer au nouveau plan d'entrainement");
					$("#div_errors").show();
				} else {
					window.location.href = "/" + locale + "/" + i18n_slug_paiement + "/" + json.plan.id;
				}
			},
			error: function (xhr, textStatus, errorThrown) {
				handle_ajax_error(textStatus, xhr, "notification_warning", "div_errors_content");
			},
		}).always(function () {
			return false;
		});
	});
});

function enregistrer_avis(afficher_confirmation) {
	var postData = {
		plan_id: $("#plan-id").val(),
		note: $("#avis-general-note").html(),
		commentaire: $("#avis-texte").val(),
	};
	$.ajax({
		type: "POST",
		url: "/api/plan/noter",
		dataType: "json",
		cache: false,
		async: true,
		timeout: 6000,
		data: {
			params: JSON.stringify(postData),
		},
		crossDomain: true,
		success: function (json) {
			if (json == undefined) {
				$("#div_errors_content").html("Impossible d'enregistrer votre avis");
				$("#div_errors").show();
			} else {
				if (afficher_confirmation) {
					$("#plan-avis-contenu-formulaire").fadeOut(0);
					$("#plan-avis-contenu-confirmation").fadeIn(500);
				}
			}
		},
		error: function (xhr, textStatus, errorThrown) {
			handle_ajax_error(textStatus, xhr, "notification_warning", "div_errors_content");
		},
	}).always(function () {
		return false;
	});
}
