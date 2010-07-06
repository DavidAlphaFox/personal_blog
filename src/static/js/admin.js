var Admin = {};

Admin.duration = 125;

Admin.common = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //
    var main_actions = null;
    var admin_menu_titles = null;

    function open_main_actions() {
        $('ul.first', main_actions).addClass('slide-down');
        $('ul.inside', main_actions).slideDown(Admin.duration);
    };

    function close_main_actions() {
        $('ul.inside', main_actions).slideUp(Admin.duration, function() {
                    $('ul.first', main_actions).removeClass('slide-down');
                });
    };

    function toggle_admin_menu() {
        $('ul', this.parentNode).slideToggle(Admin.duration);
    };

    function box(selector, class, message) {
        var box_disappear = function() {
            $(this).fadeOut(7000);
        };
        $(selector).after('<p class="' + class + ' message-box hidden">' + message + '</p>');
        $(selector + ' + p').fadeIn('slow', box_disappear);
    };

    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Admin.common.init_eventi_gui
    public.init_eventi_gui = function() {
        main_actions = $('#main-actions');
        main_actions.mouseenter(open_main_actions);
        main_actions.mouseleave(close_main_actions);

        admin_menu_titles = $('li.top-entry h4');
        admin_menu_titles.click(toggle_admin_menu);
    };

    // Admin.common.message_box
    public.message_box = function(selector, message) {
        box(selector, 'message', message);
    }

    // Admin.common.error_box
    public.error_box = function(selector, message) {
        box(selector, 'error', message);
    }

    return public;
}();

Admin.login_page = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //
    function validate_login_form() {
        // Admin.common.error_box('#login-container h2', 'Messaggio di errore');
        // return false;
        if (jQuery.trim($('#user_login').val()) == "") {
            Admin.common.error_box('#login-container h2',
                                   '<strong>ERROR</strong>: Please, insert your email address.');
            $('#user_login').addClass('red-border');
            $('#user_pwd').removeClass('red-border');
            $('#user_login').focus();
            return false;
        }

        if (jQuery.trim($('#user_pwd').val()) == "") {
            Admin.common.error_box('#login-container h2',
                                   '<strong>ERROR</strong>: Please, insert your password');
            $('#user_pwd').addClass('red-border');
            $('#user_login').removeClass('red-border');
            $('#user_pwd').focus();
            return false;
        }

        return true;
    };


    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Admin.login_page.init_eventi_gui
    public.init_eventi_gui = function() {
        $('#user_login').focus();
        $('input#submit').click(validate_login_form);
    };

    return public;
}();

Admin.dashboard_page = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //


    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Admin.dashboard_page.init_eventi_gui
    public.init_eventi_gui = function() {
    };

    return public;
}();

Admin.post_edit_page = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //

    var timestamp_status = 'ORIGINAL';
    var original_box = null;
    var original_ctime = null;
    var original_mtime = null;
    var ctime_mtime_html = [
            '<fieldset>',
            '  <legend><strong>Creation time</strong></legend>',
            '  <input type="text" id="new_ctime" name="new_ctime" value="NOT INIT" />',
            '</fieldset>',
            '<fieldset>',
            '  <legend><strong>Last modified</strong></legend>',
            '  <input type="text" id="new_mtime" name="new_mtime" value="NOT INIT" />',
            '</fieldset>'].join('\n');

    function delete_confirmation_dialog() {
        var title = $('input#post_title').val();
        // TODO: maybe a better GUI?
        return confirm('Are you sure you want to delete this post:\n' +
                       '"' + title + '"');
    };

    function change_timestamps_button_setup() {
        if (timestamp_status == 'ORIGINAL') {
            timestamp_status = 'EDIT';
            if (original_box === null) {
                original_box = $('#ts_box div.inside div#data').html();
                original_ctime = $('#original_ctime').html()
                original_mtime = $('#original_mtime').html()
            }
            $('#ts_box div.inside div#data').html(ctime_mtime_html);
            $('#ts_box div.inside div#data #new_ctime').val(original_ctime);
            $('#ts_box div.inside div#data #new_mtime').val(original_mtime);
            $('#ts_change').val('Back to original');
        } else {
            timestamp_status = 'ORIGINAL';
            $('#ts_box div.inside div#data').html(original_box);
            $('#ts_change').val('Change');
        }
    };

    function checkbox_onclick() {
        var my_name = this.name;
        if (this.checked === false) {
            $('span.tag + :checkbox[name="' + my_name + '"]').removeAttr('disabled');
        } else {
            $('span.tag + :checkbox[name="' + my_name + '"]').attr('disabled', 'disabled');
            $(this).removeAttr('disabled');
        }
    };

    function string_to_slug(str) {
        str = str.replace(/^\s+|\s+$/g, ''); // trim

        // remove accents, swap ñ for n, etc
        var from = "ÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛàáäâèéëêìíïîòóöôùúüûÑñÇç·/_,:;";
        var to   = "aaaaeeeeiiiioooouuuuaaaaeeeeiiiioooouuuunncc------";
        for (var i = 0, l = from.length ; i < l ; i++) {
            str = str.replace(new RegExp(from[i], "g"), to[i]);
        }

        str = str.replace(/[^a-zA-Z0-9 -]/g, ' '); // remove invalid chars
        str = str.replace(/^\s+|\s+$/g, '') // trim
                 .replace(/\s+/g, '-') // collapse whitespace and replace by -
                 .toLowerCase();
        return str;
    };

    function make_slug() {
        var title = $(this).val();
        $('#new_post_slug').val(string_to_slug(title));
    };


    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Admin.post_edit_page.init_eventi_gui
    public.init_eventi_gui = function() {
        $('input#delete_button').click(delete_confirmation_dialog);
        $('#ts_change').click(change_timestamps_button_setup);
        $('span.tag + :checkbox').click(checkbox_onclick);
        $('#post_title').change(make_slug);
    };

    return public;
}();

//  COMMENT FOR VIM
//  vim:set expandtab tabstop=4 shiftwidth=4 softtabstop=4 smarttab tw=100:

