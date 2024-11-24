![BlastMD](blastmd.png)

### Simple, SEO-aware static Markdown blog generator
BlastMD (pronounced "blast'em")  is a command-line tool that compiles Markdown files into HTML files, while also giving the user control of HTML tags that are important for SEO, like meta-descriptions and schema.

## Installation
In the [Releases](https://github.com/joeythornberry/blastMD/releases) tab, you can find "starter sets" that contain the blastMD executable, as well as some default HTML templates to help you get started quickly. Download and unzip the starter set, and you can run BlastMD from inside it!

## Usage
Here's how BlastMD expects your blog directory to be set up:
```
|---  md/
|     |--- Create and edit your blog posts here
|--- blog/
|     |--- This is where your blog posts end up
|     |--- Also put assets like images and CSS here
|--- templates/
|     |--- head.html
|     |--- top.html
|     |--- bottom.html
|--- post.schema
|--- blastmd
```

- You are encouraged to edit any and all of the files in the starter set. BlastMD is not a framework - it's a tool that mashes _your_ stuff together!

Essentially, BlastMD takes your Markdown files from the `md/` directory, compiles them to HTML and combines them with your template files in the `templates/` directory, and then writes the output to the `blog/` directory. Here's what that process looks like for a Markdown file called "post.md":

```
templates/head.html
+
templates/top.html
+
md/post.md
+
templates/bottom.html

= blog/post.html
```

We can then open blog/post.html in our browser, and we have a completed webpage!

I store my website's `<head>` tag in "templates/head.html", my `<header>` in "templates/top.html", and my `<footer>` in "templates/bottom.html." This way, all of my posts can have the same `<head>`, `<header>` and `<footer>`, without my having to copy~paste each one into each post. Very convenient.

## Templating Tags
You might ask, "that's all well and good, but what if my different posts need to have slightly different `<head>`s, `<header>`s and `<footer>`s?" Fortunately, I thought of a nice way to achieve that goal.

BlastMD has built-in and user-created "templating tags" that allow us to modify parts of our "head.html," "top.html," and "bottom.html" template files for each different Markdown file.

### Built-In Templating Tags
BlastMD includes some built-in tags with helpful data that you can access from your templates.
- \_Date

    The time of last modification of the .md file

- \_Home 

    Relative path to the root address of the website

- \_Url

    The relative url of the html file generated from this .md file

- \_BeginMdOnly, \_EndMdOnly

    If this markdown file has the .html extension instead of the .md extension, content and markup between these two tage will be ignored.

Inside of one of our template files, we can access these tags by using
the tag name, surrounded by the tilde character `~`, in our HTML. For
example, to use a post's date inside of its header, we go to "header.html"
and insert `~_Date~`. When we run "./blastmd," the program will replace that
tag with the date of the post. This will make sure that each post has its
own unique date.

### User-Created Templating Tags
We can also create our own templating tags and
use them in our template files. First, we tell BlastMD what 
tags we are going to use, inside the "post.schema" file. For example,
we might want to tell BlastMD that we're going to define an "Author" and
"Description" tag for each post. To achieve this, our "post.schema" should
look like this:

```
Author
Description 
```

Super easy. Now, the only thing we have to do is ensure that each 
Markdown post defines an "Author" and "Description" tag. For example,
we start "post1.md," written by Bobby Bob, with

```
Author: Bobby Bob
Description: Bobby explains how to fly a kite.
```

For "post2.md," written by Suzy Sue, we write

```
Author: Suzy Sue
Description: Suzy talks about the best birds.
```

We can now use the `~Author~` and `~Description~` tags in our HTML
templates, and they will show up in our webpages! Very nice. I use 
these templating tags to give each post its own image, title, description,
and other SEO attributes, without needing to write any HTML for 
each blog post.

## Serving the generated files
BlastMD is designed so that the `blog/` directory can be served directly by a static server such as Nginx. You can put your images and CSS in the `blog/` directory as well - BlastMD won't touch them.

Also, if you use relative imports for images and CSS, you'll be able to preview/test the fully-rendered site, exactly as it will appear online, with only your browser's built-in file viewer - no localhost needed!

## Creating Post Lists
Sometimes, we want a page to have a list of other posts. For example,
we might want our blog's homepage to have a list of recent posts, 
or we might want each post to have a list of similar posts for further
reading.

BlastMD, when run, will produce a "blog/allposts.js" file that stores all of
the built-in and user-created templating tags for each post in a single
JSON list.
We can use this to create a post list: we simply use client-side
JavaScript to fetch "allposts.js" from our server, and use it to create
an HTML post list, which we inject into the DOM. Very nice.
