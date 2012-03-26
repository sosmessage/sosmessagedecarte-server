$(document).ready(function() {
  $('.delete-app').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete this app?");
    }) ;
  });
  $('.delete-announcement').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete this announcement?");
    }) ;
  });
  $('.delete-category').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete this category?");
    }) ;
  });
  $('.delete-message').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete this message?");
    }) ;
  });
  $('.remove-category').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to remove this category from the app?");
    }) ;
  });
  $('.approve-button').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to approve this message?");
    }) ;
  });
  $('.reject-button').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to reject this message?");
    }) ;
  });
  $('.approve-all-waiting').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to approve all waiting messages?");
    }) ;
  });
  $('.delete-all-waiting').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete all waiting messages?");
    }) ;
  });
  $('.reject-all-waiting').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to reject all waiting messages?");
    }) ;
  });

  $('.approve-all-rejected').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to approve all rejected messages?");
    }) ;
  });
  $('.delete-all-rejected').each(function() {
    $(this).click(function() {
      return confirm("Are you sure you want to delete all rejected messages?");
    }) ;
  });

  $('#select-category').change(function() {
    $('#select-category option:selected').each(function () {
      window.location.href = $(this).attr("sosmessage-redirect-url");
    });
  });

  // initialize the ratings
  $('[data-rating-start]').each(function() {
    var ratingCount = $(this).attr("data-rating-count");
    if (ratingCount > 0) {
      $(this).raty({
        number: 4,
        path: "/admin/assets/images/",
        half: true,
        readOnly:  true,
        start: $(this).attr("data-rating-start"),
        width: "100%"
      });
    } else {
      $(this).raty({
        number: 4,
        path: "/admin/assets/images/",
        readOnly:  true,
        width: "100%"
      });
    }
  });

  $('#new-announcement-modal').bind('shown', function () {
    $('#new-announcement-modal input[type=text]:first').focus();
  });
  $('#new-category-modal').bind('shown', function () {
    $('#new-category-modal input[type=text]:first').focus();
  });
  $('#new-message-modal').bind('shown', function () {
    $('#new-message-modal textarea:first').focus();
  });

  // focus the first input
  if ($("form input[type=text]:first").length > 0) {
    $("form input[type=text]:first").focus();
  } else {
    $("form textarea:first").focus();
  }

  $('.color').keyup(function() {
    updateBackgroundColor($(this));
  });

  $('.color').each(function() {
    updateBackgroundColor($(this));
  });

});

function updateBackgroundColor(ele) {
  var color = $(ele).val();
  if (color[0] !== "#") {
    color = "#" + color;
    $(ele).val(color);
  }

  if (color.length > 0) {
    if (color[0] !== "#") {
      color = "#" + color;
    }
    if (color.length > 7) {
      // remove alpha
      color = "#" + color.substring(3)
    }
    $(ele).css("background-color", color);
  }
}
