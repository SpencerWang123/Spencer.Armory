#' @title Practical Tools for BA Text_mining by Spencer Wang,BFD
#'
#' @description Inside this package are many practical tools needed when we are faced with text_mining when doing BA,such as tagging,word segment,senti_analysis and SNA.etc.
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples NULL
#'
#' @export
tagdog=function(text_data,text_dataframe,tag1,tag2,tag3)
{
if(missing(text_dataframe)){print("please input 'text_dataframe'")}
if(missing(text_data)){print("please input 'text_data'")}
if(missing(tag1)){print("please input 'tag1'")}
if(missing(tag1)==T & missing(tag2)==F){print("you can't input tag2 without tag1")}
if((missing(tag1)==T|missing(tag2)==T) & missing(tag3)==F){print("you can't input tag3 without tag1 or tag2")}
text.data=text_dataframe
text.data$text=text_data
#for mac
#tag1=tag1[,-1];tag2=tag2[,-1];tag3=tag3[,-1];
####批量改变数据格式——as.character
text=as.character(text.data$text)#$后面填写文本字段名
for(i in 1:ncol(tag1)){tag1[,i]=as.character(tag1[,i])}
for(i in 1:ncol(tag2)){tag2[,i]=as.character(tag2[,i])}
for(i in 1:ncol(tag3)){tag3[,i]=as.character(tag3[,i])}
####打标签
#创建标签数据集
#思路：把一级标签打上，然后把对应一级标签下的二级标签打上，然后把对应二级标签的三级标签打上


#打一级标签###version 2.1
tag1_text=as.character(names(tag1))#读入标签文本;
tag1_text
text.data$tag1=rep("",nrow(text.data))#建立tag1标签变量
text.data$tag1_temp=rep("",nrow(text.data))#临时文本变量
text.data$tag1_log=rep(NA,nrow(text.data))#建立tag1临时逻辑变量
tag1_log_matrix=matrix(rep(NA,nrow(text.data)*ncol(tag1),ncol=ncol(tag1)),nrow=nrow(text.data))#建立tag1标签逻辑矩阵——用于一级标签数据归档以及打二级标签
summary(tag1_log_matrix)

for(i in 1:ncol(tag1))##一层：逐个一级标签展开
{
  text.data$tag1_temp=rep("",nrow(text.data))#临时文本变量归零
  text.data$tag1_log=rep("",nrow(text.data))
  tag_na_omit=as.character(tag1[,i][which(tag1[,i]!="")])
  for(j in 1:length(tag_na_omit))##二层：逐个一级标签下属关键词展开
  {
    print(paste(tag1_text[i],"----",tag1[j,i]))
    log_tag1=grepl(tag1[j,i],text)
    for(k in 1:length(log_tag1)){if(log_tag1[k]==TRUE){text.data$tag1_log[k]="selected" }}####判定是否命中关键词之一
    print(paste("至此已打标文本数",table(text.data$tag1_log)[2]))
  }
  temp_tag=tag1_text[i]
  text.data$tag1_temp[text.data$tag1_log=="selected"] =temp_tag##临时文本标签存储
  text.data$tag1=paste(text.data$tag1,text.data$tag1_temp,sep = " ")
  tag1_log_matrix[,i]=text.data$tag1_log##一级标签数据归档
  print(paste("一级指标完成度-----",i,"/",ncol(tag1)))
  print(paste(temp_tag,"已完成-----归档情况-----该标签下selected数目为：",table(tag1_log_matrix[,i])[2]))
}
print("一级指标打标签及数据归档工作完成")
if(missing(tag2))
{tagged_data=as.data.frame(cbind(text_dataframe,text.data$tag1))}
else
{
if(missing(tag3))
{
#打二级标签###v2.1
tag2_text=as.character(names(tag2))#读入二级标签文本
text.data$tag2=rep("",nrow(text.data))#建立tag2标签变量
text.data$tag2_temp=rep("",nrow(text.data))#临时文本变量
text.data$tag2_log=rep(NA,nrow(text.data))#建立tag2临时逻辑变量
tag2_log_matrix=matrix(rep(NA,nrow(text.data)),nrow=nrow(text.data))#建立tag2标签逻辑矩阵——用于二级标签数据归档以及打三级标签

for(h in 1:ncol(tag1))##将数据按照一级指标分类，后对每个一级指标下文本打二级标签
{
  tag1_temp=which(tag1_log_matrix[,h]=="selected")##提取打上某个一级标签的文本所在行
  tag12_keywords=as.data.frame(tag2[,which(grepl(tag1_text[h],tag2_text)==TRUE)]);
  if(ncol(tag12_keywords)==0)
  {print(paste(colnames(tag1)[h],"---该一级标签下无二级标签，故跳过"));print(paste("二级指标完成度-已完成第",h,"个一级指标下的二级指标，一级指标数目共有：",ncol(tag1)))}#避免某耳机标签下无二级标签，导致循环中止
  else
  {
    colnames(tag12_keywords)=names(tag2)[which(grepl(tag1_text[h],tag2_text)==TRUE)]##提取该一级标签下的二级标签关键词表
    tag2_log_mat_temp=matrix(rep(NA,nrow(text.data)*ncol(tag12_keywords)),ncol=ncol(tag12_keywords),nrow=nrow(text.data))
    colnames(tag2_log_mat_temp)=colnames(tag12_keywords)##构建二级标签逻辑矩阵——用于打三级标签以及数据归档
    for(i in 1:ncol(tag12_keywords))##一层：逐个2级标签展开
    {
      text.data$tag2_temp=rep("",nrow(text.data))#临时文本变量归零
      text.data$tag2_log=rep("",nrow(text.data))
      tag_na_omit=as.character(tag12_keywords[,i][which(tag12_keywords[,i]!="")])
      temp_tag=names(tag12_keywords)[i]
      for(j in 1:length(tag_na_omit))##二层：逐个2级标签下属关键词展开#################
      {
        print(paste(temp_tag,"----",tag12_keywords[j,i]))
        log_tag2=rep(NA,nrow(text.data))##tag2_log是一个过程逻辑变量，每换一个关键词都会被刷新
        log_tag2[tag1_temp]=grepl(tag12_keywords[j,i],text.data$text[tag1_temp])
        text.data$tag2_log[which(log_tag2==TRUE)] ="selected"##判定是否命中关键词之一
        print(paste("至此已打标文本数",table(text.data$tag2_log)[2]))
      }
      text.data$tag2_temp[which(text.data$tag2_log=="selected")]=temp_tag
      text.data$tag2 =paste(text.data$tag2,text.data$tag2_temp,sep=" ")##打标签
      tag2_log_mat_temp[,i]=text.data$tag2_log##二级标签数据归档
      print(paste(temp_tag,"-----归档情况-----该标签下selected数目为：",table(tag2_log_mat_temp[,i])[2]))
    }
    tag2_log_matrix=cbind(tag2_log_matrix,tag2_log_mat_temp)#该以及标签下的二级标签归档
    print(paste("二级指标完成度-已完成第",h,"个一级指标下的二级指标，一级指标数目共有：",ncol(tag1)))
  }
}
tag2_log_matrix=tag2_log_matrix[,-1]
print("二级指标打标及数据归档工作完成")
tagged_data=as.data.frame(cbind(text_dataframe,text.data$tag1,text_data$tag2))
}
else
{
  #打三级标签###V2.1
  tag3_text=as.character(names(tag3))#读入3级标签文本
  text.data$tag3=rep("",nrow(text.data))#建立tag3标签变量
  text.data$tag3_log=rep("",nrow(text.data))#建立tag3临时逻辑变量
  text.data$tag3_temp=rep("",nrow(text.data))#临时文本变量
  tag3_log_matrix=matrix(rep(NA,nrow(text.data)),nrow=nrow(text.data))#建立tag3标签逻辑矩阵——用于三级标签数据归档以及可能会有的四级标签

  for(h in 1:ncol(tag2_log_matrix))##将数据按照2级指标分类，后对每个3级指标下文本打3级标签
  {

    tag2_temp=which(tag2_log_matrix[,h]=="selected")##提取打上某个2级标签的文本所在行
    tag23_keywords=as.data.frame(tag3[,which(grepl(colnames(tag2_log_matrix)[h],tag3_text)==TRUE)])##提取该2级标签下的3级标签关键词表
    if(ncol(tag23_keywords)==0)
    {print(paste(colnames(tag2_log_matrix)[h],"---该二级标签下无三级标签，故跳过"));print(paste("三级指标完成度-已完成第",h,"个二级指标下的三级指标，二级指标数目共有：",ncol(tag2)))}#避免某耳机标签下无三级标签，导致循环中止
    else
    {
      colnames(tag23_keywords)=names(tag3)[which(grepl(colnames(tag2_log_matrix)[h],tag3_text)==TRUE)]##提取该二级标签下的三级标签关键词表
      tag3_log_mat_temp=matrix(rep(NA,nrow(text.data)*ncol(tag23_keywords)),ncol=ncol(tag23_keywords),nrow=nrow(text.data))
      colnames(tag3_log_mat_temp)=colnames(tag23_keywords)##构建3级标签逻辑矩阵——用于打4级标签以及数据归档
      for(i in 1:ncol(tag23_keywords))##一层：逐个3级标签展开
      {
        text.data$tag3_temp=rep("",nrow(text.data))#临时文本变量归零
        text.data$tag3_log=rep("",nrow(text.data))
        tag_na_omit=as.character(tag23_keywords[,i][which(tag23_keywords[,i]!="")])
        temp_tag=names(tag23_keywords)[i]
        for(j in 1:length(tag_na_omit))##二层：逐个3级标签下属关键词展开#################
        {
          print(paste(temp_tag,"----",tag23_keywords[j,i]))
          log_tag3=rep(NA,nrow(text.data))##tag3_log是一个过程逻辑变量，每换一个关键词都会被刷新
          log_tag3[tag2_temp]=grepl(tag23_keywords[j,i],text.data$text[tag2_temp])
          text.data$tag3_log[which(log_tag3==TRUE)] ="selected"##判定是否命中关键词之一
          print(paste("至此已打标文本数",table(text.data$tag3_log)[2]))
        }
        text.data$tag3_temp[which(text.data$tag3_log=="selected")]=temp_tag
        text.data$tag3 =paste(text.data$tag3,text.data$tag3_temp,sep=" ")##打标签
        tag3_log_mat_temp[,i]=text.data$tag3_log##二级标签数据归档
        print(paste(temp_tag,"-----归档情况-----该标签下selected数目为：",table(tag3_log_mat_temp[,i])[2]))
      }
      tag3_log_matrix=cbind(tag3_log_matrix,tag3_log_mat_temp)#该以及标签下的二级标签归档
      print(paste("三级指标完成度-已完成第",h,"个二级指标下的三级指标，二级指标数目共有：",ncol(tag2)))
    }
  }
  tag3_log_matrix=tag3_log_matrix[,-1]
  print("三级指标打标及数据归档工作完成")
tagged_data=as.data.frame(cbind(text_dataframe,text.data$tag1,text_data$tag2,text_data$tag3))
}
}
tagged_data$tag1=gsub(" +"," ",tagged_data$tag1)
tagged_data$tag1=gsub("$ +","",tagged_data$tag1)
tagged_data$tag1=gsub("^ +","",tagged_data$tag1)
tagged_data$tag2=gsub(" +"," ",tagged_data$tag2)
tagged_data$tag2=gsub("$ +","",tagged_data$tag2)
tagged_data$tag2=gsub("^ +","",tagged_data$tag2)
tagged_data$tag3=gsub(" +"," ",tagged_data$tag3)
tagged_data$tag3=gsub("$ +","",tagged_data$tag3)
tagged_data$tag3=gsub("^ +","",tagged_data$tag3)
print("打标签后文档见——")
}

