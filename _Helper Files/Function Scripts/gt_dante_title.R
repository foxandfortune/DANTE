gt_dante_title <- function(title,
                           subtitle,
                           value = NULL,
                           logo_link = NULL,
                           filepath,
                           type = c('mbb', 'wbb'),
                           title_font_size = 24,
                           title_font_weight = 'bold',
                           title_lineheight = 0.5,
                           subtitle_font_size = 16,
                           subtitle_font_weight = 'normal',
                           subtitle_lineheight = 0.5,
                           logo_height = 65) {
  
  # logo link is an optional parameter in case a user wants to plot a different logo (e.g. retro)
  link <- if(!is.null(logo_link)) {
    logo_link
  } else if(type == 'wbb') {
    paste0("data:", "image/png", ";base64,",base64enc::base64encode(glue::glue("{filepath}Beatrice.png")))
  } else if (type == 'mbb') {
    paste0("data:", "image/png", ";base64,",base64enc::base64encode(glue::glue("{filepath}Virgil.png")))
  } 
  
  title_header <- glue::glue(
    "<div style='display: flex; justify-content: space-between; align-items: center;'>
     <div style='flex-grow: 1;'>
       <span style='font-weight: {title_font_weight}; font-size: {title_font_size}px; line-height: {title_lineheight};'>{title}</span><br>
       <span style='font-size: {subtitle_font_size}px; font-weight: {subtitle_font_weight}; line-height: {subtitle_lineheight};'>{subtitle}</span>
     </div>
     <div>
       <img src='{link}' style='height: {logo_height}px; width: auto; vertical-align: middle; border-radius: 15px; border: 2px solid #000000'>
     </div>
   </div>"
  )
  
  return(title_header)
  
}

saveRDS(gt_dante_title, '_Helper Files/Simulation Functions/gt_dante_title.rds')
