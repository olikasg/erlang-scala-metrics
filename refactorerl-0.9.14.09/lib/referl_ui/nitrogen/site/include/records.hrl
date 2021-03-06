-record(textarea_autocomplete, {?ELEMENT_BASE(element_textarea_autocomplete), tag, text="", minLength=2, delay=300, html_encode=true, next, postback, delegate=undefined }).
-record(notification, {?ACTION_BASE(action_notification), tag, text="", type="error", html_encode=true }).
-record(highlight_source, {?ACTION_BASE(action_highlight_source), targetId, startPos, endPos}).
-record(resize_textarea, {?ACTION_BASE(action_resize_textarea), targetId, maxWidth}).
-record(toggle_access_to_db, {?ACTION_BASE(action_toggle_access_to_db), enable=true}).
