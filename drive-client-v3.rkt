#lang racket

(require net/url)
(require (planet ryanc/webapi:1:=1/oauth2))
(require json)
(require net/uri-codec)

; interface to Google Drive including collecting folders with sub-folders,
; and searching over a list of folders.
; uses Drive API v3
;
; Fred Martin, fredm@cs.uml.edu, Feb 15 2016
;
; to use:
; first set up the authentication token (see below),
; then:
; 
; list-all-folders takes a single folder ID (string)
;  and returns a list of drive#file folders contained within it.
;
; (map get-id <list of drive#file objs>) will produce a list of the string IDs,
;  which can be a parameter for search-complete-folder.
; 
; search-complete-folder takes a list of folder ID strings and a search phrase (may be empty string)
;  and returns list of drive#file objects matching search
; 
; (display-titles <list of drive#file objs>) will print each file's title and URL to it.
;
; examples
; assume foo is a list of folder ID strings to search
;   (define myfolders (map get-id (list-all-folders "<folder-id-string>")))
;   (search-complete-folder myfolders "") => all files in those folders
;   (search-complete-folder myfolders "'fredm@cs.uml.edu' in owners")
;   (search-complete-folder myfolders "fullText contains fredm")
;   (search-complete-folder myfolders "modifiedTime > '2015-10-01'")
; etc.
; see https://developers.google.com/drive/v3/web/search-parameters
;
;   (define mysearch (search-complete-folders myfolders "modifiedTime > '2015-10-01'"))
;   (display-titles mysearch)
;
; existing bugs:
; * if the list of folder ID strings is too long, search-complete-folder will fail
;   because the constructed URL gets too long.
;
; * if the search comes up empty (usually because of a malformed search query),
;   there will be a runtime error.
;
; to improve:
; there are five sorts of arg/return types:
; - obj ID string
; - list of obj ID strings (e.g. list of folder ID strings)
; - drive#file obj (with slots for id, kind, mimeType, and name)
; - list of drive#file objs
; - drive#fileList obj, as returned by direct API calls (e.g. all-drive-files and inside various procs),
;   (with slots for kind, files, and nextPageToken)
; it's hard keeping straight which procedure accepts which thing.

; to go Google Developers Console and make a project
; go to Dashboard > Explore other services > Enable APIs and get credentials like keys
; then Google Apps APIs > Drive API (and enable it)
; then in the left-column menu, Credentials
; then Create credentials > Oauth client ID > Other
; then copy the client ID and secret into this procedure.
(define drive-client
  (oauth2-client
   #:id "548798434144-6s8abp8aiqh99bthfptv1cc4qotlllj6.apps.googleusercontent.com"
   #:secret "<email me for secret if you want to use my API>"))

; to get an access token to use the drive-client:
; 
; set renew to true
; evaluate buffer and you should get a redirect to your system web browser
; approve permissions in web browser
; sometimes it fails to communicate back with the temporary localhost web server
; that gets launched
; keep trying until it succeeds, then evaluate:
;   (send myoauth2 headers)
; and copy the new bearer token into the else position of the (if renew...) statement.
; then set renew to false.

; alternately
; evaluate this:
;   (send google-auth-server get-auth-request-url #:client drive-client #:scopes '("https://www.googleapis.com/auth/drive"))
; go to the URL provided and authorize permissions
; then copy the auth code into this and evaluate it:
;   (send (oauth2/auth-code google-auth-server drive-client "<auth-code-here>") headers)
; and copy the new bearer string into the token setting below.
(define renew #f)

(define myoauth2 '())
(define token '())

(if renew
    (begin 
      (set! myoauth2 (oauth2/request-auth-code/browser
                        google-auth-server
                        drive-client
                        '("https://www.googleapis.com/auth/drive")
                        ))
      (set! token (send myoauth2 headers)))
    (set! token '("Authorization: Bearer ya29.lgJMRfkaypKseG9HMIeZzp4AuuKDy5YLfUolEdHSwAwmoqSepSuUVe3hSyyG1rw27g")))

; have to do (send myoauth2 validate!) before (send myoauth2 get-scopes) will work.

; THIS IS THE ONE THAT YOU USE
; given a single folder ID string, returns list of enclosed subfolder drive#file objects
; recurses into subfolders all the way down
; map get-id onto this result before handing to search-complete-folder.
(define (list-all-folders folder-id)
  (let ((this-level (list-folders folder-id)))
    (begin
      (display (length this-level)) (display "... ")
      (append this-level
              (flatten (map list-all-folders (map get-id this-level)))))))

; returns list of drive#file objs
(define (list-folders folder-id)
  (filter folder? (list-all-children folder-id)))

; filter a list of drive#file objs with this
(define (folder? drive-file)
  (string=? (hash-ref drive-file 'mimeType "nope") "application/vnd.google-apps.folder"))


; THIS IS THE ONE THAT YOU USE
; recursive version to repeatly call below to get through all pages
; give list of 1 or more folder ID strings, returns list of drive#file objects
; recursively call self as necessary to step through pages of results
(define (search-complete-folder folder-ids query . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                      (search-folder folder-ids query)
                      (search-folder folder-ids query (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
              (search-complete-folder folder-ids query page-token))
        (get-files this-page))))

; produces a drive#fileList object
; which has a list of drive#file objects
(define (search-folder folder-ids query . next-page-token)
 (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?"
                                "q="
                                (form-urlencoded-encode
                                 (folder-query folder-ids))
                                "+and+"
                                (form-urlencoded-encode (string-append "(" query ")"))
                                "&key=" (send drive-client get-id)
                                (if (= 1 (length next-page-token))
                                    (string-append "&pageToken=" (car next-page-token))
                                    "")
;                                "&pageSize=3"
                                ))
    token)))

(define (folder-query folder-list)
  (define (folder-query-iter folder-list result)
    (if (null? folder-list)
        result
        (folder-query-iter (cdr folder-list)
                           (string-append result
                                          " or "
                                          "'" (car folder-list) "'"
                                          " in parents"))))
  (string-append "('" (car folder-list) "' in parents"
                 (if (= 1 (length folder-list)) ""
                     (folder-query-iter (cdr folder-list) ""))
                 ")"))



; THIS IS THE ONE YOU USE
; given single folder ID string, returns list of enclosed drive#file objects
; recursively calls self to step through pages of results and to process subfolders
(define (list-all-children folder-id . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                      (list-children folder-id)
                      (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
              (list-all-children folder-id page-token))
        (get-files this-page))))

; produces a drive#fileList object
; which has a list of drive#file objects
(define (list-children folder-id . next-page-token)
  (read-json
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?"
                                "q='" folder-id "'+in+parents"
                                "&key=" (send drive-client get-id)
                                (if (= 1 (length next-page-token))
                                    (string-append "&pageToken=" (car next-page-token))
                                    "")
;                                "&pageSize=5"
                                ))
    token)))

(define (get-files obj)
  (hash-ref obj 'files))

(define (get-id obj)
  (hash-ref obj 'id))



; returns drive#fileList object
; files are in 'files slot; retrieve with get-files
(define (all-drive-files)
  (read-json 
   (get-pure-port
    (string->url (string-append "https://www.googleapis.com/drive/v3/files?key="
                                (send drive-client get-id)
                                "&pageSize=10"
                                ))
    token)))

;(define my-items (hash-ref drive-files 'items))

; accepts a list of file objects and URLs to open
(define (display-titles file-list)
  (cond ((not (null? file-list))
         (begin
           (let ((item (car file-list)))
;             (display "kind: ")
;             (display (hash-ref item 'kind))
;             (display " mimeType: ")
;             (display (hash-ref item 'mimeType))
;             (display " title: ")
             (display (hash-ref item 'name))
             (display (string-append "\thttps://drive.google.com/open?id="
                                     (get-id item)))
             (display "\n")
             (display-titles (cdr file-list)))))))
; e.g.
; (display-titles (map child-to-file (get-files (list-children "<folder-id-string>"))))
