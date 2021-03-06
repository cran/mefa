`mefadocs` <-
function (doc = c("ChangeLog","SampleReport")) 
{
    doc <- match.arg(doc)
    if (length(grep(".pdf", doc)) > 0) {
        doc <- file.path(system.file(package = "mefa"), "doc", 
            doc)
        if (.Platform$OS.type == "windows") 
            shell.exec(doc)
        else system(paste(getOption("pdfviewer"), doc, "&"))
    }
    else {
        file.show(system.file(package = "mefa", doc))
    }
}

