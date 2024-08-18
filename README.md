# BlastMD
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

Essentially, BlastMD takes your Markdown files from the `md/` directory, compiles them to HTML and combines them with your template files in the `templates/` directory, and then writes the output to the `blog/` directory.

Additionally, you can add metadata information like titles, authors, and descriptions to the start of your markdown files. BlastMD will extract that metadata and insert it into your templates as needed, i.e., you start you .md file with "Title: My Post", and BlastMD will insert "My Post" into your template files wherever it sees "~Title~". Use the tilde character (~) to surround words that you want to replace.

All posts must provide the same kinds of metadata, as defined in the post.schema file. I.e., if your post.schema has "Image" in it, but your post doesn't define what image it wants, BlastMD will throw an exception.

## Serving the generated files

BlastMD is designed so that the `blog/` directory can be served directly by a static server such as Nginx.

If you use relative imports for external files like images and CSS, you'll be able to view the fully-rendered site, exactly as it will appear online, with only your browser's built-in file viewer - no localhost needed!
