# HTML('
#     <div class="ui slider checkbox">
#       <input id = "switch" type="checkbox" name="newsletter">
#       <label>Accept terms and conditions</label>
#     </div>
# ')  




# shiny_ui$attribs$class <- paste(custom_input_class, class)
# shiny_ui$attribs$id <- input_id
# shiny_ui$attribs[["data-value"]] <- selected
# shiny_ui$attribs[["data-value-type"]] <- type




new_input <- function(){
  
  HTML('<div class="ui form">
  <div class="grouped fields">
     <label>Outbound Throughput</label>
     <div class="field">
     <div class="ui slider checkbox">
     <input type="radio" name="throughput" checked="checked">
     <label>20 mbps max</label>
     </div>
     </div>
     <div class="field">
     <div class="ui slider checkbox">
     <input type="radio" name="throughput">
     <label>10mbps max</label>
     </div>
     </div>
     <div class="field">
     <div class="ui slider checkbox">
     <input type="radio" name="throughput">
     <label>5mbps max</label>
     </div>
     </div>
     <div class="field">
     <div class="ui slider checkbox checked">
     <input type="radio" name="throughput">
     <label>Unmetered</label>
     </div>
     </div>
     </div>
     </div>')
  
}

checkbox_input <- function(id, label = NULL, fitted = F, checked = F, class = NULL){
  
  fit <- NULL; check <- NULL
  
  if(fitted) fit <- "fitted"
  if(checked) check <-  "checked"
  
  variation <- paste("ui", fit, "checkbox", sep = " ")
  
  # override default ui class
  if(!is.null(class)) variation <- class
  
  tagList(
    htmltools::div(class = variation,
                   htmltools::tags$input(id = id, type = "checkbox", checked = check),
                   htmltools::tags$label(label)
    )
  )
}
