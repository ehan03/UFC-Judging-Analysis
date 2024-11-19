#' ---
#' title: "R Report Template"
#' date: "Last Updated: October 2023"
#' author: "Jay Emerson"
#' output: html_document
#' # Or use output: pdf_document, ...
#' # Or don't use either, in which case you will be prompted to choose
#' ---
#' 
#' We can't cover everything.  Here's some usefull stuff (by example). Some code
#' and results might appear in the report.  Others might be excluded.
#' We also have _emphasis_ or __more emphasis__ or
#' ___even more emphasis___ possible, all following the style of R Markdown.
#' 
#' # This is a big section
#' 
#' ## This is a sub-section
#' 
#' ### This is a sub-sub-section
#' 
#' 
#' # Standard report organization
#' 
#' You don't have to do this exactly, but this is a good starting point.
#' Here's what I would envision for sections:
#' 
#' 0. Not even a section: ABSTRACT.  A short paragraph that summarizes
#' your topic and the key contribution you are making.
#' 
#' 1. INTRODUCTION.  Give an overview.  Why is this an important topic?
#' What aspects of the topic are you focusing on?  What questions do you
#' hope to answer or what data will you explore?  What is some background
#' and what prior work has been done?   Briefly, where are
#' the data from?  And describe to the reader how the rest of the report
#' is organized.  Like: "Section 2 describes the data, and 
#' the data processing and merging.  Section 3 presents our
#' exploration and analysis. ..."
#' 
#' 2. THE DATA. Fully describe the data and what you did to clean/process/merge.
#' Give examples of a few lines (perhaps not all variables).  Maybe
#' even show a basic plot or two if it helps tell the story so the reader
#' can better understand the data.
#' 
#' 3. RESULTS. Probably your exploration and analysis, the most important and
#' longest part!  This could have sub-sections.  If you build a shiny app,
#' you can include screenshots and discuss how it works.
#' 
#' 4. CONCLUSION.  Summarize what you did and what you found.  Talk about
#' limitations of your work, and topics for possible further research.
#' 
#' 5. References/citations.  This is important!  Be careful not to copy
#' and use material from the web without formally citing it!  Also:
#' Cite R itself!
#'
#' POSSIBLY: Some reports might have a METHODOLOGY section before the
#' RESULTS.  It depends on the topic, though.
#' 
#' We also have the ability to do bullet points:
#' 
#' - Point 1
#' - Point 2
#' - Point 3
#' 
#' But in general, you can do a search for R Markdown or even just 
#' Markdown, and most of what you find will work here in the
#' "hashtag-prime" formatting areas such as on these lines.
#' 
#'
#' How to cite R: Here's what you need to know:

citation()

#'
#' # Integrating R code
#'
#' In order for this section to make sense, you need to compare the
#' following code to the HTML file produced with you compile the report.
#' A nice report should include useful, helpful information and results,
#' but is not just showing your script.  It needs to look close to a
#' professional journal article.  Not too much, not too little.

# This is a code comment, not for paragraph discussion in a report.
# This, the code, and the results will be integrated into the report if
# you do nothing.  You shouldn't show too much code in the report unless
# you are explaining the code in a way that makes it important for the
# reader to see it.

x <- rnorm(100)
hist(x)               # R graphics are shown easily!
summary(x)

# The following range(x) will be shown as code, but the code won't
# actually be run and there is no result shown.  This is unlikely to
# be used in your report:

#+ eval = FALSE
range(x)            # Shown, not run

# The following minus sign changes back to the default display
# and execution characteristics, so things that follow are presented and
# run normally.

#-

max(x)              # Run and results shown as usual

# In the following chunk, the result (a minimum) is shown
# but the code is excluded.  This is nice behavior for more
# professional reports.  You want to show and discuss the result
# of running some code, but without actually showing the code.

#+ echo = FALSE

min(x)

#-

mean(x)        # Run and shown as usual

#+ include = FALSE

y <- runif(100)     # This is run, but nothing is shown in this chunk
print(y)            # Also not shown!  This is very useful.

#-

cat("mean of y is", mean(y), "\n")

#' Now we're back to normal.  I think the most useful one is `include = FALSE`
#' where you need the code to run (perhaps cleaning data and creating other
#' objects), but don't want to show the code or any results.
#' 
#' 
#' # Example of including a picture in your report
#' 
#' The image file needs to be in the same directory as the report.
#' 
#' ![Caption for the picture.](IMG_0847.jpg){width="400px"}


# And I added something here, and now I have to reformat.  Here I am writing a
# comment and the comment is long and getting longer andabout to be too long.
# There.  Now I can continue...  This is a comment and this is not a paragraph.

#' This is a paragraph.
#' This would continue the same paragraph and look nice
#' in the
#' output even though
#' it looks odd here.
#' 
#' # An example with math!
#' 
#' $$ X = \frac{\sum_{i=1}^N emissions_i}{\sum_{i=1}^N production_i} $$
#' 
#' 
