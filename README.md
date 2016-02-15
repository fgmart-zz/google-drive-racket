# google-drive-racket
Recursively make list of subfolders in a Google Drive folder, and then search them. Using Scheme (racket-lang.org).

I started this project in response to this thread on the Google Drive help forum, where we all were annoyed that you can't limit a search to a folder (and including all subfolders that it contained).

As I figured out what's needed to do this, it's obvious why:

1. You have to recursively decend through subfolders, collecting all files that have the root folder as a parent (and filtering them for files that are folders)
2. Then you can construct a search query that includes a list of all the subfolders (flattened into a single list)
3. Only this isn't the full solution, because if you have too many subfolders, it doesn't fit into the URL of one search query.

So this is my implementation. It works, but #3 isn't done yet.
