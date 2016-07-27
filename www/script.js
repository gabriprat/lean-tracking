$(document).ready(function(){
    $(".fixme").sticky({topSpacing:10});
    $("input").change(function() {
      saveFormToCookie("form");
    });
    
    $("input").each(function() {
      $(this).attr("name", $(this).attr("id"));
    });
    
    loadFormFromCookie();
});

/*!
 * jQuery Cookie Plugin v1.4.1
 * https://github.com/carhartl/jquery-cookie
 *
 * Copyright 2006, 2014 Klaus Hartl
 * Released under the MIT license
 */
(function (factory) {
	if (typeof define === 'function' && define.amd) {
		// AMD (Register as an anonymous module)
		define(['jquery'], factory);
	} else if (typeof exports === 'object') {
		// Node/CommonJS
		module.exports = factory(require('jquery'));
	} else {
		// Browser globals
		factory(jQuery);
	}
}(function ($) {

	var pluses = /\+/g;

	function encode(s) {
		return config.raw ? s : encodeURIComponent(s);
	}

	function decode(s) {
		return config.raw ? s : decodeURIComponent(s);
	}

	function stringifyCookieValue(value) {
		return encode(config.json ? JSON.stringify(value) : String(value));
	}

	function parseCookieValue(s) {
		if (s.indexOf('"') === 0) {
			// This is a quoted cookie as according to RFC2068, unescape...
			s = s.slice(1, -1).replace(/\\"/g, '"').replace(/\\\\/g, '\\');
		}

		try {
			// Replace server-side written pluses with spaces.
			// If we can't decode the cookie, ignore it, it's unusable.
			// If we can't parse the cookie, ignore it, it's unusable.
			s = decodeURIComponent(s.replace(pluses, ' '));
			return config.json ? JSON.parse(s) : s;
		} catch(e) {}
	}

	function read(s, converter) {
		var value = config.raw ? s : parseCookieValue(s);
		return $.isFunction(converter) ? converter(value) : value;
	}

	var config = $.cookie = function (key, value, options) {

		// Write

		if (arguments.length > 1 && !$.isFunction(value)) {
			options = $.extend({}, config.defaults, options);

			if (typeof options.expires === 'number') {
				var days = options.expires, t = options.expires = new Date();
				t.setMilliseconds(t.getMilliseconds() + days * 864e+5);
			}

			return (document.cookie = [
				encode(key), '=', stringifyCookieValue(value),
				options.expires ? '; expires=' + options.expires.toUTCString() : '', // use expires attribute, max-age is not supported by IE
				options.path    ? '; path=' + options.path : '',
				options.domain  ? '; domain=' + options.domain : '',
				options.secure  ? '; secure' : ''
			].join(''));
		}

		// Read

		var result = key ? undefined : {},
			// To prevent the for loop in the first place assign an empty array
			// in case there are no cookies at all. Also prevents odd result when
			// calling $.cookie().
			cookies = document.cookie ? document.cookie.split('; ') : [],
			i = 0,
			l = cookies.length;

		for (; i < l; i++) {
			var parts = cookies[i].split('='),
				name = decode(parts.shift()),
				cookie = parts.join('=');

			if (key === name) {
				// If second argument (value) is a function it's a converter...
				result = read(cookie, value);
				break;
			}

			// Prevent storing a cookie that we couldn't decode.
			if (!key && (cookie = read(cookie)) !== undefined) {
				result[name] = cookie;
			}
		}

		return result;
	};

	config.defaults = {};

	$.removeCookie = function (key, options) {
		// Must not alter options, thus extending a fresh object...
		$.cookie(key, '', $.extend({}, options, { expires: -1 }));
		return !$.cookie(key);
	};

}));


/* 
 * Save Form To Cookie
 * (cc) Paul Philippov, themactep@gmail.com
 * 
 * This is rather a proof of concept than a production-ready solution.
 * It does not handle radio buttons and such.
 * You might want to extend it to suit your needs.
 * 
 * Usage:
 *
 * $(function(){
 *  var myForm = $('#formid');
 *  loadFormFromCookie(myForm);
 *  myForm.submit(function() {
 *    saveFormToCookie(this);
 *  });
 * });
 */

(function ($) {
    $.fn.serializeJSON = function () {
        var json = {};
        jQuery.map($(this).serializeArray(), function (n, _) {
            json[n['name']] = n['value'];
        });
        return json;
    };
})(jQuery);

ensureNumber = function (n) {
    n = parseInt(n, 10);
    if (isNaN(n) || n <= 0) {
        n = 0;
    }
    return n;
};

saveFormToCookie = function () {
    var data = JSON.stringify($("form").serializeJSON());
    $.cookie("form", data, {expires: 365});
};

loadFormFromCookie = function () {
    var data = $.cookie("form");

    if (typeof data === 'undefined') {
        return;
    }

    JSON.parse(data, function (key, value) {
        if (typeof (value) !== 'object') {
            var el = $("form").find('*[name="' + key + '"]');

            if (el.is('input')) {
                if (false) {
                    // code formatting stub
                } else if (el.attr('type') === 'number') {
                    el.val(ensureNumber(value));
                } else if (el.attr('type') === 'checkbox') {
                    el.attr('checked', value === 'on' ? true : false);
                } else if (el.attr('type') === 'radio') {
                    el.filter('[value="' + value.replace(/(['"])/g, "\\$1") + '"]').attr("checked", true);
                } else if (el.attr('type') === 'file') {
                    
                } else {
                    el.val(value);
                }
            } else if (el.is('select')) {
                el.val(value);
            } else if (el.is('textarea')) {
                el.val(value);
            }
        }
    });
};