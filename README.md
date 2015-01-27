# Receive In

You want to see how your site behaves if one of your *assets* or *AJAX call* takes a looong time to load.

`Receive In` can help.

`Receive In` is a delay proxy which supports CORS.


----

**Update**: It turns out that what I was trying to do via CORS header is [explicitly not allowed in the spec](http://stackoverflow.com/questions/24135854/why-does-cors-specification-not-allow-redirects). However, the spec _may_ allow this behavior if a compelling use-case comes up.
