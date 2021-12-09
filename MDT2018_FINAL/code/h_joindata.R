## ----- Process Demographics Data -----
demogr_pre17 <- read.csv('../data/ANON_Demographics12to16.csv', header=T, stringsAsFactors=F)
demogr_17 <- read.csv('../data/ANON_Demographics17.csv', header=T, stringsAsFactors=F)
demogr_mfp <- read.csv('../data/ANON_Demographics13to17MFP.csv', header=T, stringsAsFactors=F)
demogr_17 <- demogr_17[complete.cases(demogr_17),]

merge_demogr <- function (...) {
    df <- list(...)
    
    df <- lapply(df, function (x) {
        subset(x, select = c("AnonID",
                             "Programme.Of.Study.Sought.Title",
                             "Gender",
                             "Domicile.on.Programme.Entry",
                             "Fee.Status.Grouping"))
    })
    
    # Merge the two demographics datasets----
    # df <- do.call(rbind, df) # slow
    df <- rbind.fill(df)
    
    # Remove duplicate rows;
    df <- df[!duplicated(df$AnonID),]
    
    # Change legacy fee status groupings to current;
    # Change as required----
    df$Fee.Status.Grouping[df$Fee.Status.Grouping == 'HOME'] <- 'RUK'
    df$Fee.Status.Grouping[df$Fee.Status.Grouping == 'SEU'] <- 'EU'
    df$Fee.Status.Grouping[df$Fee.Status.Grouping == 'OVERSEAS'] <- 'Overseas'
    df$Fee.Status.Grouping[df$Domicile.on.Programme.Entry == 'Scotland'] <- 'Scotland'

    # Make Fee.Status.Grouping a factor;
    df$Fee.Status.Grouping <- factor(df$Fee.Status.Grouping, levels = c('Overseas', 'EU', 'RUK', 'Scotland'))
    
    # subset demographics data;
    df <- subset(df, select=c('AnonID', 'Programme.Of.Study.Sought.Title', 'Gender', 'Fee.Status.Grouping'))
    
    # Change column names;
    colnames(df) <- c("AnonID","School","Gender", "Region")
    
    # Find Maths, Comp Sci and Physics majors----
    mathsMajors <- grep("^(Mathematics|Applied)", df$School) 
    physMajors <- grep("^(Physics|Astro|Mathematical|Theoretical)", df$School) 
    econMajors <- grep("^Economics", df$School) 
    compMajors  <- grep("^(Artificial|Computer|Informatics)", df$School)
    
    # Add Major column to demographics.df----
    df$School <- rep("Other", nrow(df))
    df$School[mathsMajors] <- "Mathematics"
    df$School[physMajors] <- "Physics"
    df$School[econMajors] <- "Economics"
    df$School[compMajors] <- "Informatics"
    df$School <- factor(df$School,
                          levels = c("Mathematics",
                                     "Physics",
                                     "Informatics",
                                     "Economics",
                                     "Other"))
    
    df
}

demogr <- merge_demogr(demogr_pre17, demogr_17, demogr_mfp)

rm(demogr_pre17, demogr_17, demogr_mfp)


## ----- Process Course Results -----
courses <- read.csv('../data/ANON_CourseResults13to17.csv', header=T)

reshape_course_result <- function (df) {
    # NO = Non-assessed (class only)
    # WD = Withdrawn
    # AN = Absent
    df <- subset(df, !(Assessment.Grade %in% c('AN', 'NO', 'WD')))
    
    # Keep results of first sit, unless first sit has been removed (e.g. AN)
    row_tobe_rm <- c()
    for (i in 1:nrow(df)) {
        if (df$Assessment.Attempt.Number[i] == 2) {
            id <- df$AnonID[i]
            name <- df$Course.Name[i]
        } else {
            next
        }

        firstsit <- subset(df, Course.Name==name & AnonID==id & Assessment.Attempt.Number==1)
        if (!nrow(firstsit)) {
            # Assume second sit as first sit if AN (which has been removed)
            df$Assessment.Attempt.Number[i] == 1
        } else {
            # Otherwise remove second sit
            row_tobe_rm <- c(row_tobe_rm, i)
        }
    }
    df <- df[-row_tobe_rm, ]
    
    # Order df such that courses taking early are placed above courses taking later, given same students
    df <- df[order(df$AnonID, df$Course.Year),]
    # Remove duplicated row (re-take courses)
    df <- df[!duplicated(df[c('AnonID', 'Course.Name')]),]
    # Uncomment next line to cross-check rows removed with original csv
    # return(df)
    
    # 'Unmelt' data frame
    df <- dcast(df, AnonID~Course.Name, value.var='Assessment.Mark')
    
    df <- subset(df, select = c('AnonID', 
                         'Accelerated Algebra and Calculus for Direct Entry', 
                         'Accelerated Proofs and Problem Solving', 
                         'Calculus and its Applications', 
                         'Introduction to Linear Algebra', 
                         'Mathematics for Physics 1', 
                         'Proofs and Problem Solving', 
                         'Several Variable Calculus and Differential Equations')
               )
    colnames(df) <- c('AnonID', 'AAC', 'APPS', 'CAP', 'ILA', 'MfP1', 'PPS', 'SVCDE')
    
    df
}

reshape_course_result2 <- function (df) {
    # NO = Non-assessed (class only)                   - remove
    # WD = Withdrawn                                   - remove
    # AN = Absent                                      - keep
    # FF = Force fail                                  - keep
    # ES = Credits awarded due to special circumstance - keep
    # NS = Null sit                                    - remove
    df <- subset(df, !(Assessment.Grade %in% c('NO', 'WD', 'NS')))
    
    # For Pre2017 students
    df <- subset(df, Course.Year!='2017/8')
    # Order df such that courses taking early are placed above courses taking later, given same students
    df <- df[order(df$AnonID, df$Course.Year),]
    # Remove duplicated row (re-take courses)
    df <- df[!duplicated(df[c('AnonID', 'Course.Name')]),]
    # Uncomment next line to cross-check rows removed with original csv
    # return(df)
    
    # 'Unmelt' data frame
    df_unmelt <- dcast(df, AnonID~Course.Name, value.var='Assessment.Mark')
    df_unmelt <- subset(df_unmelt, select = c('AnonID', 
                         'Accelerated Algebra and Calculus for Direct Entry', 
                         'Accelerated Proofs and Problem Solving', 
                         'Calculus and its Applications', 
                         'Introduction to Linear Algebra', 
                         'Mathematics for Physics 1', 
                         'Proofs and Problem Solving', 
                         'Several Variable Calculus and Differential Equations')
               )
    colnames(df_unmelt) <- c('AnonID', 'AAC', 'APPS', 'CAP', 'ILA', 'MfP1', 'PPS', 'SVCDE')
    
    df_unmelt_grade <- dcast(df, AnonID ~ Course.Name, value.var='Assessment.Grade')
    df_unmelt_grade <- subset(df_unmelt_grade, select = c('AnonID', 
                         'Accelerated Algebra and Calculus for Direct Entry', 
                         'Accelerated Proofs and Problem Solving', 
                         'Calculus and its Applications', 
                         'Introduction to Linear Algebra', 
                         'Mathematics for Physics 1', 
                         'Proofs and Problem Solving', 
                         'Several Variable Calculus and Differential Equations'))
    df_unmelt_grade <- df_unmelt_grade[rowSums(is.na(df_unmelt_grade))!=7,]
    colnames(df_unmelt_grade) <- c('AnonID', 'AAC.Grade', 'APPS.Grade', 'CAP.Grade', 'ILA.Grade', 'MfP1.Grade', 'PPS.Grade', 'SVCDE.Grade')
    df_unmelt_grade_grade <- lapply(df_unmelt_grade[2:8], function (x) {
        x[which(x %in% c('A1','A2','A3'))] <- 'A'
        x[which(x=='FF')] <- 'E'
        x[which(x=='ES')] <- 'D'
        x <- factor(x, levels=c(LETTERS[1:8],'AN'))
    })
    df_unmelt_grade_grade <- do.call(data.frame, df_unmelt_grade_grade)
    df_unmelt_grade <- cbind(AnonID=df_unmelt_grade$AnonID, df_unmelt_grade_grade)
    
    merge(df_unmelt, df_unmelt_grade, by='AnonID', all = T)
}

reshape_course_result17 <- function (df) {
    # NO = Non-assessed (class only)                   - remove
    # WD = Withdrawn                                   - remove
    # AN = Absent                                      - keep
    # FF = Force fail                                  - keep
    # ES = Credits awarded due to special circumstance - keep
    # NS = Null sit                                    - remove
    df <- subset(df, !(Assessment.Grade %in% c('NO', 'WD', 'NS')))
    
    # Order df such that courses taking early are placed above courses taking later, given same students
    df <- df[order(df$AnonID, df$Course.Year),]
    # Remove duplicated row (re-take courses)
    df <- df[!duplicated(df[c('AnonID', 'Course.Name')]),]
    # Uncomment next line to cross-check rows removed with original csv
    # return(df)
    
    # 'Unmelt' data frame
    df_unmelt <- dcast(df, AnonID ~ Course.Name + Course.Year, value.var='Assessment.Mark')
    df_unmelt <- subset(df_unmelt, select = c('AnonID', 
                         'Accelerated Algebra and Calculus for Direct Entry_2017/8', 
                         'Accelerated Proofs and Problem Solving_2017/8', 
                         'Calculus and its Applications_2017/8', 
                         'Introduction to Linear Algebra_2017/8', 
                         'Mathematics for Physics 1_2017/8', 
                         'Proofs and Problem Solving_2017/8', 
                         'Several Variable Calculus and Differential Equations_2017/8')
               )
    
    df_unmelt <- df_unmelt[rowSums(is.na(df_unmelt))!=7,]
    colnames(df_unmelt) <- c('AnonID', 'AAC', 'APPS', 'CAP', 'ILA', 'MfP1', 'PPS', 'SVCDE')
    
    df_unmelt_grade <- dcast(df, AnonID ~ Course.Name + Course.Year, value.var='Assessment.Grade')
    df_unmelt_grade <- subset(df_unmelt_grade, select = c('AnonID', 
                         'Accelerated Algebra and Calculus for Direct Entry_2017/8', 
                         'Accelerated Proofs and Problem Solving_2017/8', 
                         'Calculus and its Applications_2017/8', 
                         'Introduction to Linear Algebra_2017/8', 
                         'Mathematics for Physics 1_2017/8', 
                         'Proofs and Problem Solving_2017/8', 
                         'Several Variable Calculus and Differential Equations_2017/8'))
    df_unmelt_grade <- df_unmelt_grade[rowSums(is.na(df_unmelt_grade))!=7,]
    colnames(df_unmelt_grade) <- c('AnonID', 'AAC.Grade', 'APPS.Grade', 'CAP.Grade', 'ILA.Grade', 'MfP1.Grade', 'PPS.Grade', 'SVCDE.Grade')
    df_unmelt_grade_grade <- lapply(df_unmelt_grade[2:8], function (x) {
        x[which(x %in% c('A1','A2','A3'))] <- 'A'
        x[which(x=='FF')] <- 'E'
        x[which(x=='ES')] <- 'D'
        x <- factor(x, levels=c(LETTERS[1:8],'AN'))
    })
    df_unmelt_grade_grade <- do.call(data.frame, df_unmelt_grade_grade)
    df_unmelt_grade <- cbind(AnonID=df_unmelt_grade$AnonID, df_unmelt_grade_grade)
    
    merge(df_unmelt, df_unmelt_grade, by='AnonID', all = T)
}

courses17 <- reshape_course_result17(courses)
courses <- reshape_course_result2(courses)

# match_exam_score <- function (data, exam, name) {
#   if (name == "ILA") fullname <- "Introduction to Linear Algebra"
#   if (name == "CAP") fullname <- "Calculus and its Applications"
#   if (name == "PPS") fullname <- "Proofs and Problem Solving"
#   if (name == "MfP1") fullname <- "Mathematics for Physics 1"
#   if (name == "AAC") fullname <- "Accelerated Algebra and Calculus for Direct Entry"
#   if (name == "APPS") fullname <- "Accelerated Proofs and Problem Solving"
#   if (name == "SVCDE") fullname <- "Several Variable Calculus and Differential Equations"
#   data[, name] <- NA
#   for (i in 1:nrow(data)) {
#     rnum <- which(exam$AnonID==as.character(data$AnonID[i]) &
#                     exam$Course.Name==fullname &
#                     exam$Assessment.Grade!="WD" & 
#                     exam$Assessment.Grade!="AN")
#     if (length(rnum) > 1) rnum <- rnum[which.max(exam$Assessment.Mark[rnum])]
#     if (length(rnum)) {data[, name][i] <- exam$Assessment.Mark[rnum]}
#   }
#   data
# }


## ----- Merge DT scores with demographics data and course scores -----
joint_pre17 <- merge(mdt_clean, courses, by='AnonID', all=T)
joint_17 <- merge(mdt17_clean, courses17, by='AnonID', all=T)

joint_pre17 <- merge(joint_pre17, demogr, by='AnonID', all.x=T)
joint_17 <- merge(joint_17, demogr, by='AnonID', all.x=T)


## ----- Add coloumns of indicators -----
# Indicators of: 
#     is taking both ILA and MfP1
#     is taking both CAP and MfP1
#     is taking both PPS and MfP1
#     pass or fail in ILA
#     grade in ILA
#     number of Y1 maths courses taken (commented out)
#     number of Y2 maths courses taken (commented out)
#     number of 'Fail' in Y1 maths courses
#     number of 'Fail' in Y2 maths courses
add_course_indic <- function (df) {
    df$ILA.MfP1 <- df$CAP.MfP1 <- df$PPS.MfP1 <- df$ILA.bin <- NA_character_
    
    df$ILA.MfP1[!is.na(df$ILA)] <- 'No'
    df$ILA.MfP1[complete.cases(df[, c('ILA', 'MfP1')])] <- 'Yes'
    
    df$CAP.MfP1[!is.na(df$CAP)] <- 'No'
    df$CAP.MfP1[complete.cases(df[, c('CAP', 'MfP1')])] <- 'Yes'
    
    df$PPS.MfP1[!is.na(df$PPS)] <- 'No'
    df$PPS.MfP1[complete.cases(df[, c('PPS', 'MfP1')])] <- 'Yes'
    
    factor(df$ILA.MfP1, exclude=NULL)
    factor(df$CAP.MfP1, exclude=NULL)
    factor(df$PPS.MfP1, exclude=NULL)
    
    # df$Y1Maths <- rowSums(!is.na(df[,c("ILA", "CAP", "PPS")]))
    # df$Y2Maths <- rowSums(!is.na(df[,c("AAC", "APPS", "SVCDE")]))
    
    df$Y1Fs <- rowSums(df[,c("ILA", "CAP", "PPS")] < 50)
    df$Y2Fs <- rowSums(df[,c("AAC", "APPS", "SVCDE")] < 50)
    
    df$ILA.bin[!is.na(df$ILA)] <- 'Fail'
    df$ILA.bin[which(df$ILA >= 50)] <- 'Pass'
    factor(df$ILA.bin, exclude=NULL)
    
    df$CAP.bin[!is.na(df$CAP)] <- 'Fail'
    df$CAP.bin[which(df$CAP >= 50)] <- 'Pass'
    factor(df$CAP.bin, exclude=NULL)
    
    df$PPS.bin[!is.na(df$PPS)] <- 'Fail'
    df$PPS.bin[which(df$PPS >= 50)] <- 'Pass'
    factor(df$PPS.bin, exclude=NULL)
    
    grades <- cut(df$ILA, c(0,40,60,101), include.lowest=T, right=F)
    grades <- mapvalues(grades, from=c('[0,40)', '[40,60)', '[60,101]'), to=c('E','CD','AB'))
    factor(grades, exclude=NULL)
    df$ILA.grade <- grades
    
    df
}

joint_pre17 <- add_course_indic(joint_pre17)
joint_17 <- add_course_indic(joint_17)


## ----- Process Entry Qualifications -----
entryquals <- read.csv('../data/ANON_entryquals17.csv', header=T, stringsAsFactors=F)

for (i in 1:nrow(entryquals)) {
    if (grepl('A-Level', entryquals$Maths.Grade[i])) {
        entryquals$Qual[i] <- 'A-Level'
        if (entryquals$Maths.Grade[i] == 'A-Level A*') {
            entryquals$Category[i] <- 'Cat3'
        } else if (entryquals$Maths.Grade[i] == 'A-Level A' & entryquals$Further.Maths.Grade[i] %in% c('FM A-Level A', 'FM A-Level A*')) {
            entryquals$Category[i] <- 'Cat2'
        } else {
            entryquals$Category[i] <- 'Cat1'
        }
    } else if (grepl('AH', entryquals$Maths.Grade[i])) {
        entryquals$Qual[i] <- 'SQA'
        if (grepl('AH A', entryquals$Maths.Grade[i])) {
            entryquals$Category[i] <- 'Cat3'
        } else if (grepl('AH B', entryquals$Maths.Grade[i])) {
            entryquals$Category[i] <- 'Cat2'
        } else {
            entryquals$Category[i] <- 'Cat1'
        }
    } else if (grepl('AS', entryquals$Maths.Grade[i])) {
        entryquals$Qual[i] <- 'A-Level'
        entryquals$Category[i] <- 'Cat1'
    } else if (grepl('Higher', entryquals$Maths.Grade[i])) {
        entryquals$Qual[i] <- 'SQA'
        entryquals$Category[i] <- 'Cat1'
    } else if (grepl('IB', entryquals$Maths.Grade[i])) {
        entryquals$Qual[i] <- 'IB'
        if (grepl('SL', entryquals$Maths.Grade[i])) {
            entryquals$Category[i] <- 'Cat1'
        } else if (grepl('H5', entryquals$Maths.Grade[i])) {
            entryquals$Category[i] <- 'Cat2'
        } else {
            entryquals$Category[i] <- 'Cat3'
        }
    } else {
        entryquals$Qual[i] <- 'Unknown'
        entryquals$Category[i] <- 'Unknown'
    }
}

joint_17 <- merge(joint_17, entryquals[c('AnonID', 'Qual', 'Category')], by='AnonID', all.x=T)
joint_17$Qual[which(is.na(joint_17$Qual))] <- 'Unknown'
joint_17$Category[which(is.na(joint_17$Category))] <- 'Unknown'