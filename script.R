# 读取 My Clippings.txt
text <- read.csv("My Clippings.txt", header = FALSE)

library(tidyverse)
library(lubridate)
library(cli)

# 第一行加上 kindle 默认的分行符，方便后续处理
text <- data.frame(V1 = "==========") |> rbind(text)

# 将笔记整理为 tidy data
text2 <- text %>% 
  as_tibble() %>% 
  mutate(group = case_when(
    V1 == "==========" ~ "split",
    lag(V1) == "==========" ~ "title",
    str_detect(V1, "^- 您在") ~ "info",
    TRUE ~ "text"
  ),
  group = if_else(group == "text" &
                  lag(group) == "title" &
                  lead(group) == "info",
                  "subtitle", group)) %>% 
  filter(!group %in% c("split", "subtitle")) %>% 
  mutate(id = case_when(
    group == "title" ~ row_number()
  )) %>% 
  fill(id, .direction = "down") %>% 
  pivot_wider(names_from = group,
              values_from = V1) %>% 
  mutate(across(c(title, info), unlist),
         text = map_chr(text, str_c, collapse = "\n"))

# 处理title
# 书名是将导出的文件名，应注意不得含有.等特殊符号
# 可以利用下面的代码针对性修改书名，如不需要，可以注释掉这些代码
# 以后导入 kindle 注意书名噢（从根源解决问题）
text2 <- text2 %>% 
  mutate(title = case_when(
    str_detect(title, "彷徨少年时") ~ "德米安：彷徨少年时（赫尔曼·黑塞）",
    str_detect(title, "少即是多") ~ "少即是多（本田直之）",
    str_detect(title, "东方之旅") ~ "东方之旅（赫尔曼·黑塞）",
    str_detect(title, "成为我自己") ~ "成为我自己：欧文·亚隆回忆录（欧文·亚隆）",
    TRUE ~ title
  ))


# 从 `info` 中提取笔记起始、结束位置、页码、时间等信息
# 添加行号
text3 <- text2 %>% 
  mutate(page = str_extract(info, "\\d+(?= 页)"),
         begin = str_extract(info, "(?<=#)\\d+"),
         end = str_extract(info, "(?<=-)\\d+"),
         date = str_extract(info, "\\d*年\\d*月\\d*日"),
         time = str_extract(info, "\\d*:\\d*:\\d*"),
         afternoon = str_detect(info, "下午"),
         datetime = ymd_hms(paste(date, time)),
         datetime = case_when(
           afternoon == TRUE ~ datetime + hours(12),
           afternoon == FALSE & hour(datetime) == 12 ~ datetime - hours(12), 
           TRUE ~ datetime
         )) %>% 
  select(title, text, datetime, page, begin, end) %>% 
  filter(text != "") %>% 
  mutate(across(4:6, as.numeric)) %>% 
  group_by(title) %>% 
  arrange(begin, datetime, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(id = row_number())

# 去重，界定重复的算法可以改进
# 通过判断，得到可行笔记行号
# 2022-09-12 似乎判断起始位置就可以
del <- text3 %>% 
  filter(begin == lead(begin)) %>% 
  pull(id)

#### 最终数据框 ####
result <- text3[-del, 1:6]

# 形成嵌套数据框
dfs <- result %>% 
  mutate(print = str_c(text, " （", datetime, "）")) %>% 
  select(title, print) %>% 
  group_split(title, .keep = FALSE)

# 构建输出路径
titles <- result %>% 
  distinct(title) %>% 
  pull(title)
files <- str_c("files/", titles, ".md")

# 批量输出，大功告成
for (i in 1:length(titles)) {
  write_lines(dfs[[i]]$print,
              file = files[i],
              sep = "\n\n")
  cli_alert_success("Write {titles[i]}.")
}
