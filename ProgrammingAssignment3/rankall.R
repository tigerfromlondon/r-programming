rankall <- function(outcome, num = "best") {
        my_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
        if (outcome %in% names(outcomes) == FALSE) {
                print("invalid outcome")
                stop()
        }
        sub <- my_data[, c(2, 7, outcomes[outcome])]
        names(sub) <- c("hos","stat","out")
        com <- na.omit(sub)
        sor <- com[order(com$stat,com$out,com$hos),]
        spl <- split(sor, sor$stat)
        sapply(spl, function(x) {
                t <- subset(x, select = hos:stat)
                names(t) <- c("hospital", "state")
                #po <- as.data.frame(spl)
                if(num == "best") {
                        num <- 1
                }
                if(num == "worst") {
                        num <- nrow(t)
                }
                z <- as.data.frame(t[num,])
                print(z, row.names = FALSE, col.names = FALSE) 
        })
}