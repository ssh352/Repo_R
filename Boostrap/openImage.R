#
#   Support code: Open and close an image
#
openImage = function(nm) {
    if (SAVE_IMAGES) {
        fname = paste0(nm,".svg")
        fpath = file.path("images", fname)
        svg(fpath, onefile=FALSE,
            height=7, width=11.3)
    }
}

closeImage = function() {
    if (SAVE_IMAGES) {
        dev.off()
    }
}
