modes = function(x) {
        # Return all modes
        ux = unique(x)
        tab = tabulate(match(x, ux))
        ux[tab == max(tab)]
}

