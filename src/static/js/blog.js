// PUBLIC PAGES JS CODE

var Blog = {};

Blog.common = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //

    // --------
    // SCRAMBLE
    // --------
    // This code is intentionally scrambled to fool spam bots
    var last = '';
    var zvkpunefmap;
    var havpbqr_ng = '\x40';
    var cebgbpby = zvkpunef('znvygb:');

    function zvkpunefinit()
    {
        var fsdfretvokwervwjfd = new Array();
        var s = 'abcdefghijklmnopqrstuvwxyz';

        for (i = 0; i < s.length; i++)
            fsdfretvokwervwjfd[s.charAt(i)] = s.charAt((i+13)%26);

        for (i = 0; i < s.length; i++)
            fsdfretvokwervwjfd[s.charAt(i).toUpperCase()] = s.charAt((i+13)%26).toUpperCase();

        return fsdfretvokwervwjfd;
    }

    function zvkpunef(a)
    {
        if (!zvkpunefmap)
            zvkpunefmap = zvkpunefinit();

        s = '';
        for (i = 0; i < a.length; i++)
        {
            var b = a.charAt(i);
            s += (b >= 'A' && b <= 'Z' || b >= 'a' && b <= 'z' ? zvkpunefmap[b] : b);
        }
        return s;
    }
    // -------- END OF SCRAMBLE

    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Blog.common.init_gui_events
    public.init_gui_events = function() {
        $('div#old-browser-warning').click(function() {
            $('div#old-browser-warning').slideUp('slow');
        });

        $('#footer').css('display', 'none');
        $('#footnotes').css('display', 'none');

        $('#footer-band').toggle(
            function() {
                $('#footnotes').show();
                $('#footer').show('fast', function() {
                    $.scrollTo('#footer-band', 'fast');
                });
            },
            function() {
                $('#footer').slideUp('slow');
                $('#footnotes').slideUp('slow');
            }
        );

        return true;
    };

    // Blog.common.scramble
    public.scramble = function (a, b, c, d) {
        var c = zvkpunef(c);
        var qngn = zvkpunef(a + havpbqr_ng + b);
        $('span.' + zvkpunef(c)).before(zvkpunef('<n pynff="' + zvkpunef(c) + '"></n>')).remove();
        $('a.' + c).attr(zvkpunef('uers'), cebgbpby + qngn);
        $('a.' + c).html(zvkpunef(d));
    }

    return public;
}();

Blog.single_post = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //
    function comment_preview() {
        var div = $('#comment-preview');

        var post_data = {
            'comment_text' : $('#comment').val()
        };

        var callback = function (responseText, textStatus, XMLHttpRequest) {
            // this; // dom element
            var jThis = $(this);
            jThis.slideDown('slow');
        }

        $('input').removeClass('red-border');
        $('textarea').removeClass('red-border');

        if (!div.is(':hidden')) {
            div.slideUp('slow', function() {
                div.load('/comment-preview', post_data, callback);
                });
        }
        else {
            div.load('/comment-preview', post_data, callback);
        }
        $('textarea#comment' + name).focus();

        return false; // to prevent form submission
    };

    function validate_field(name) {
        if (jQuery.trim($('#' + name).val()) == "") {
            $('input').removeClass('red-border');
            $('textarea').removeClass('red-border');
            $('#' + name).addClass('red-border');
            $('#' + name).focus();
            return false;
        }
    };

    function validate_comment_form() {
        if (validate_field('author') === false  ||
            validate_field('email') === false   ||
            validate_field('comment') === false) {
            return false;
        }
        else {
            return true;
        }
    };

    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Blog.single_post.init_gui_events
    public.init_gui_events = function() {
        $('input#preview-comment').click(comment_preview);
        $('input#submit-comment').click(validate_comment_form);
        Blog.common.scramble('c.qbanqrb', 'gnhgbybtvpn.bet', 'cnbyb', 'Cnbyb Qbanqrb');
        return true;
    };

    return public;
}();

Blog.multi_post = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //

    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Blog.multi_post.init_gui_events
    public.init_gui_events = function() {
        Blog.common.scramble('c.qbanqrb', 'gnhgbybtvpn.bet', 'cnbyb', 'Cnbyb Qbanqrb');
        return true;
    };

    return public;
}();

Blog.pages = function() {
    // ====================================================================== //
    //                             PRIVATE PARTS                              //
    // ====================================================================== //

    // ====================================================================== //
    //                             PUBLIC  PARTS                              //
    // ====================================================================== //
    var public = {};

    // Blog.pages.init_gui_events
    public.init_gui_events = function() {
        Blog.common.scramble('c.qbanqrb', 'gnhgbybtvpn.bet', 'cnbyb', 'Cnbyb Qbanqrb');
        Blog.common.scramble('c.qbanqrb', 'gnhgbybtvpn.bet', 'cnbyb2', 'c.qbanqrb@gnhgbybtvpn.bet');
        return true;
    };

    return public;
}();

//  vim:set expandtab tabstop=4 shiftwidth=4 softtabstop=4 smarttab tw=100:

