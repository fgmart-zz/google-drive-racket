# Interface to Google Drive in Racket

## Fred Martin
### April 22, 2017

# Overview
This set of code provides an interface to searching through one's Google Drive account. 
Its most important feature is that it provides a *folder-delimited search*.

The essential model of files in Google Drive is that they are in one big “pile.” So you can't directly find a file in a
given folder.

This code recursively collects all folders found within a given folder, and then 
construct a search query that includes a list of all the subfolders (flattened into a single list).

This then allows you to perform a folder-delimited search.

**Authorship note:** All of the code described here was written by myself.

# Libraries Used
The code uses four libraries:

```
(require net/url)
(require (planet ryanc/webapi:1:=1/oauth2))
(require json)
(require net/uri-codec)
```

* The ```net/url``` library provides the ability to make REST-style https queries to the Google Drive API.
* Ryan Culpepper's ```webapi``` library is used to provide the ```oauth2``` interface required for authentication.
* The ```json``` library is used to parse the replies from the Google Drive API.
* The ```net/uri-codec``` library is used to format parameters provided in API calls into an ASCII encoding used by Google Drive.

# Key Code Excerpts

Here is a discussion of the most essential procedures, with a focus on **how they embody ideas from 
UMass Lowell's COMP.3010 Organization of Programming languages course.** 

Five examples are shown and they are individually numbered. The titles of each section highlight the course concepts embodied.

## 1. Initialization using a Global Object

The following code creates a global object, ```drive-client``` that is used in each of the subsequent API calls:

```
(define drive-client
  (oauth2-client
   #:id "548798434144-6s8abp8aiqh99bthfptv1cc4qotlllj6.apps.googleusercontent.com"
   #:secret "<email me for secret if you want to use my API>"))
 ```
 
 While using global objects is not a central theme in the course, it's necessary to show this code to understand
 the later examples.
 
## 2. Selectors and Predicates using Procedural Abstraction

A set of procedures was created to operate on the core ```drive-file``` object. Drive-files may be either
actual file objects or folder objects. In Racket, they are represented as a hash table.

```folder?``` accepts a ```drive-file```, inspects its ```mimeType```, and returns ```#t``` or ```#f```:

```
(define (folder? drive-file)
  (string=? (hash-ref drive-file 'mimeType "nope") "application/vnd.google-apps.folder"))
```

Another object produced by the Google Drive API is a list of drive-file objects ("```drive#fileList```"). 
When converted by the JSON library,
this list appears as hash map. 

```get-files``` retrieves a list of the files themselves, and ```get-id``` retrieves the unique ID
associated with a ```drive#fileList``` object:

```
(define (get-files obj)
  (hash-ref obj 'files))

(define (get-id obj)
  (hash-ref obj 'id))
```
## 3. Using Recursion to Assemble Results

The low-level routine for interacting with Google Drive is named ```list-children```. This accepts an ID of a 
folder object, and optionally, a token for which page of results to produce.

A lot of the work here has to do with pagination. Because it's a web interface, one can only obtain a page of
results at a time. So it's necessary to step through each page. When a page is returned, it includes a token
for getting the next page. The ```list-children``` just gets one page:

```
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
```
The interesting routine is ```list-all-children```. This routine is directly invoked by the user.
It optionally accepts a page token; when it's used at top level this parameter will be null.

The routine uses ```let*``` to retrieve one page of results (using the above ```list-children``` procedure)
and also possibly obtain a token for the next page.

If there is a need to get more pages, the routine uses ```append``` to pre-pend the current results with 
a recursive call to get the next page (and possibly more pages).

Ultimately, when there are no more pages to be had, the routine terminates and returns the current page. 

This then generates a recursive process from the recursive definition.

```
(define (list-all-children folder-id . next-page-token)
  (let* ((this-page (if (= 0 (length next-page-token))
                      (list-children folder-id)
                      (list-children folder-id (car next-page-token))))
         (page-token (hash-ref this-page 'nextPageToken #f)))
    (if page-token
        (append (get-files this-page)
              (list-all-children folder-id page-token))
        (get-files this-page))))
```

## 4. Filtering a List of File Objects for Only Those of Folder Type

The ```list-all-children``` procedure creates a list of all objects contained within a given folder.
These objects include the files themselves and other folders.

The ```filter``` abstraction is then used with the ```folder?``` predicate to make a list of subfolders
contained in a given folder:

```
(define (list-folders folder-id)
  (filter folder? (list-all-children folder-id)))
```

## 5. Recursive Descent on a Folder Hierarchy

These procedures are used together in ```list-all-folders```, which accepts a folder ID and recursively
obtains the folders at the current level and then recursively calls itself to descend completely into the folder
hierarchy.

```map``` and ```flatten``` are used to accomplish the recursive descent:

```
(define (list-all-folders folder-id)
  (let ((this-level (list-folders folder-id)))
    (begin
      (display (length this-level)) (display "... ")
      (append this-level
              (flatten (map list-all-folders (map get-id this-level)))))))
```
