(function($) {

    var jsTweetSource, jsTweetBody, jsUserTranslation, jsGoogleTranslation, form, submitButton;

    var author, time;
    var isComplete;

    $(document).ready(function() {
        jsTweetSource = $('.js-tweet-source');
        if (jsTweetSource.length) {
            jsTweetBody = $('.js-tweet-body');
            jsUserTranslation = $('.js-user-translation');
            jsGoogleTranslation = $('.js-google-translation');
            form = $('form');
            form.submit(submit);
            submitButton = form.find('button');
            getLatestTweet();
        }
    });

    
    var getLatestTweet = function() {
        $.get('translation', function(data) {
            author = data.author;
            time = data.time;
            jsTweetSource.text(data.author);
            jsTweetBody.text(data.tweet);
            jsUserTranslation.val('');
            jsGoogleTranslation.hide();
            jsGoogleTranslation.find('p').text(data.translation);

        });
    };

    var submit = function(event) {
        event.preventDefault();
        if (isComplete) {
            isComplete = false;
            submitButton.text('Submit');
            getLatestTweet();
        } else {
            //Do the submission
            $.post('translation/'+author+'/'+time, {'translation': jsUserTranslation.val()}, function() {
                isComplete = true;
                jsGoogleTranslation.show();
                submitButton.text('Next');
            });
        }

    };
})(jQuery);
