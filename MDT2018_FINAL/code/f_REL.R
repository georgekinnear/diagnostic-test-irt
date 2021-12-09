stack_prop_bar <- function (data, year, breaks, plots=T) {
    year_char <- paste0("Y",year,"Fs")
    df <- data[, c("AnonID", "Total", year_char)]
    df <- df[complete.cases(df), ]
    
    # Append column of interval
    df$Interval <- cut(df$Total, breaks)
    
    # Calculate frequencies and proportion, given interval
    data.count <- table(Interval=df$Interval, Fs=df[,year_char])
    df.freq <- data.frame(data.count)
    df.prop <- data.frame(prop.table(data.count, margin=1))
    
    df <- merge(df.freq, df.prop, by=c("Interval", "Fs"))
    # Reorder by factor Fs in descending order
    df <- arrange(df, Interval, rev(Fs))
    
    # Calculate position of label
    df <- ddply(df, .(Interval), transform, pos = cumsum(Freq.y) - (0.5 * Freq.y))

    if (plots) {
        ggplot(data=df, aes(Interval, Freq.y, fill=Fs)) +
            geom_bar(stat="identity", position="fill") +
            geom_text(data=df, aes(Interval, pos, label=paste0(round(Freq.y * 100, 1), "%")), size=4) +
            scale_fill_brewer(palette='RdYlGn', direction=-1, name='# of \'Fail\'') + 
            scale_y_continuous(labels=percent_format()) + 
            labs(x='Diagnostic Test', y='Proportion')
    } else {
        addmargins(data.count)
    }
}

split_boxplot <- function (df, course.name, school='Mathematics', cut=c(0,65,100), latex=F, xlab='MDT') {
    # Filter data frame
    df <- subset(df, School==school, select=c('Total', course.name, paste0(course.name, '.MfP1')))
    df <- df[complete.cases(df), ]
    
    # Append column of interval
    df$Interval <- cut(df$Total, cut)
    
    # ANOVA
    fml <- paste0(course.name, ' ~ ', course.name, '.MfP1')
    if (latex) {
        print(xtable(aov(as.formula(fml), data=subset(df, Interval==levels(Interval)[1])), digits=c(0,0,0,2,3,3)))
    } else {
        print(summary(aov(as.formula(fml), data=subset(df, Interval==levels(Interval)[1]))))
    }
    
    # Calculate frequencies for annotation
    freq <- table(df$Interval, df[,paste0(course.name, '.MfP1')])
    freq.df <- as.data.frame(freq)
    
    ggplot(df, aes_string(x='Interval', y=course.name)) + 
        geom_boxplot(aes_string(fill=paste0(course.name, '.MfP1'))) + 
        geom_text(data=freq.df, aes(x=Var1, y=105, label=Freq, group=Var2), position=position_dodge(0.8)) + 
        coord_cartesian(ylim=c(0,105)) + 
        labs(fill='Also take MfP1?', 
             title=paste0(course.name), 
             x=xlab, 
             y='Course Result')
}

stack_prop_tiles <- function (df, xname, yname, title=NULL, xlab=NULL, ylab='Proportion', fac=NULL) {
    arguments <- as.list(match.call())
    xval <- eval(arguments$xname, df)
    yval <- eval(arguments$yname, df)
    
    if (is.null(xlab)) xlab <- as.character(arguments$xname)
    if (is.null(fac)) fac <- as.character(arguments$yname)
    
    count <- table(yval, xval)
    df <- as.data.frame(prop.table(count, margin=2), responseName='h')
    
    wval <- prop.table(colSums(count))
    xposval <- cumsum(wval) - 0.5 * wval
    
    df <- merge(df, data.frame(xval=names(wval), w=wval, xpos=xposval), by='xval', all=T)

    df <- ddply(df, .(xval), transform, ypos=1-cumsum(h)+0.5*h)
    
    print(xtable(addmargins(count), digits=rep(0,ncol(count)+2), align=c('r',rep('c',ncol(count)+1))))
    
    ggplot(df, aes(xpos, ypos, fill=yval)) + 
        geom_tile(aes(width = (w-0.01) * (w-0.01>0), 
                      height = (h-0.01) * (h-0.01>0))) + 
        scale_fill_brewer(palette='RdYlGn', direction=-1) + 
        scale_x_continuous(breaks=xposval, minor_breaks=NULL) + 
        scale_y_continuous(labels=percent_format()) +
        labs(title=title, x=xlab, y=ylab, fill=fac) + 
        theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
}