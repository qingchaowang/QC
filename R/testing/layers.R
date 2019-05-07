layer_delegate <- setRefClass("layer_delegate",
                              
                              methods = list(
                                claimed = function(amount){}
                              ))

layer <- setRefClass("layer",
                     
                     fields = list(
                       size = "numeric",
                       reinstatement = "ANY",
                       delegates = "list"
                     ),
                     
                     methods = list(
                       claim = function(amount, retention) {
                         this_loss <- pmin(size, pmax(amount - retention, 0))
                         size <<- size - this_loss
                         for(d in delegates) d$claimed(this_loss)
                         
                         if(!is.null(reinstatement)) {
                           reinstated_amount <- reinstatement$claim(this_loss, 0)
                           size <<- size + reinstated_amount
                         }
                         
                         return (this_loss)
                       }
                     ))

loss_aggregator <- setRefClass("loss_aggregator", 
                               
                               fields = list(
                                 total = "numeric"
                               ),
                               
                               methods = list(
                                 claimed = function(amount) {
                                   total <<- total + amount
                                 }
                               ),
                               
                               contains = "layer_delegate")

tower <- setRefClass("tower", 
                     
                     fields = list(
                       layers = "list"
                     ),
                     
                     methods = list(
                       claim = function(amount) {
                         
                         retention <- 0

                         for(i in 1:length(layers)) {

                           tmp_size <- layers[[i]]$size
                           layers[[i]]$claim(amount, retention)
                           retention <- retention + tmp_size
                         }
                       }
                     ))

