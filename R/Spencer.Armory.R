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
###
tagdog=function(all_data,all_data_text,tag_level1,tag_level2,tag_level3)
{
#####
if(missing(all_data)){stop('你必须输入一个需要打标签的文档，汪汪汪！')}else{if (mode(all_data)!="list") stop('all_data应该是一个数据表，汪汪汪！')}
if(missing(tag_level1)){stop('你必须输入一个一级标签表-tag_level1，汪汪汪！')}else{if (mode(tag_level1)!="list") stop('tag_level1应该是一个数据表，汪汪汪！')}
if(missing(all_data_text)){stop('你必须输入一串要打标签的字符向量，汪汪汪！')}
text.data=all_data
tag1=tag_level1
####


####
#for mac
#tag1=tag1[,-1];tag2=tag2[,-1];tag3=tag3[,-1];
####批量改变数据格式——as.character
text=as.character(all_data_text)#$后面填写文本字段名
text.data$text=all_data_text
for(i in 1:ncol(tag1)){tag1[,i]=as.character(tag1[,i])}


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
text.data$tag1=gsub(" +"," ",text.data$tag1)
text.data$tag1=gsub("$ +","",text.data$tag1)
text.data$tag1=gsub("^ +","",text.data$tag1)
##输出文档
tagged_data=as.data.frame(cbind(all_data,text.data$tag1))
colnames(tagged_data)=c(colnames(all_data),"一级标签")
print("一级指标打标签及数据归档工作完成")
print("===========================================标签等级分割线==============================================")
print("=======================================================================================================")



####
#打二级标签###v2.1
###标签格式
if(missing(tag_level2))
{
  write.csv(tagged_data,"tagged_data.csv");
  stop("无二级标签，故终止打标签，汪汪汪！打好的标签文档tagged_data已经写入你的工作空间啦！快去看文件")
  }
else
  {
    if (mode(tag_level2)!="list") 
      {stop('tag_level2应该是一个数据表')}
else
{
tag2=tag_level2
for(i in 1:ncol(tag2)){tag2[,i]=as.character(tag2[,i])}
###
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
text.data$tag2=gsub(" +"," ",text.data$tag2)
text.data$tag2=gsub("$ +","",text.data$tag2)
text.data$tag2=gsub("^ +","",text.data$tag2)
##输出文档
tagged_data=as.data.frame(cbind(all_data,text.data$tag1,text.data$tag2))
colnames(tagged_data)=c(colnames(all_data),"一级标签","二级标签")
print("二级指标打标及数据归档工作完成")
print("===========================================标签等级分割线==============================================")
print("=======================================================================================================")
}
}

####


####
#打三级标签###V2.1
###标签格式
if(missing(tag_level3))
{
  write.csv(tagged_data,"tagged_data.csv")
stop("无三级标签，故终止打标签，汪汪汪！打好的标签文档tagged_data已经写入你的工作空间啦！快去看文件")
  }
else
  {
    if (mode(tag_level3)!="list")
      {stop('tag_level3应该是一个数据表')}
else
{
tag3=tag_level3
for(i in 1:ncol(tag3)){tag3[,i]=as.character(tag3[,i])}
###
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
print("===========================================标签等级分割线==============================================")
print("=======================================================================================================")
print("标签文档见==tagged_data")
text.data$tag3=gsub(" +"," ",text.data$tag3)
text.data$tag3=gsub("$ +","",text.data$tag3)
text.data$tag3=gsub("^ +","",text.data$tag3)
###
#输出文档
tagged_data=as.data.frame(cbind(all_data,text.data$tag1,text.data$tag2,text.data$tag3))
colnames(tagged_data)=c(colnames(all_data),"一级标签","二级标签","三级标签")
write.csv(tagged_data,"tagged_data.csv")
warning("汪！我占领了这篇文档的所有电线杆！三级标签均已打完，打好的标签文档tagged_data已经写入你的工作空间啦！快去看文件")

}
}


####
rm(tag1)
rm(tag1_log_matrix)
rm(tag12_keywords)
rm(tag2)
rm(tag2_log_matrix)
rm(tag23_keywords)
rm(tag3)
rm(tag3_log_matrix)
rm(tag2_log_mat_temp)
rm(tag3_log_mat_temp)
rm(text.data)
#
rm(h)
rm(i)
rm(j)
rm(k)
rm(log_tag1)
rm(log_tag2)
rm(log_tag3)
rm(tag_na_omit)
rm(tag1_temp)
rm(tag1_text)
rm(tag2_temp)
rm(tag2_text)
rm(tag3_text)
rm(temp_tag)
rm(text)
}
###



spider_also_speaker=function(text_vector,nonscence_word)
{
#preparation
library(jiebaRD);library(jiebaR)
matchstick=worker("tag")
set_fire_on_cigar=function(x){segment(x,matchstick,mod = "mix")}
noscence_word=as.character(nonscence_word)
text_vector=as.character(text_vector)
#segment
wcof_data=lapply(text_vector,set_fire_on_cigar)
text_relation=wcof_data
#words
fenci_all=unlist(wcof_data);
fenci_unique0=unique(fenci_all[names(fenci_all)=="n"|names(fenci_all)=="a"|names(fenci_all)=="v"])
fenci_unique=fenci_unique0[(fenci_unique0 %in% noscence_word)==FALSE]
#matrix
a=rep(0,length(fenci_unique)^2)
data_matrix=matrix(a,nrow =length(fenci_unique),ncol = length(fenci_unique))
colnames(data_matrix)=fenci_unique
rownames(data_matrix)=fenci_unique
#work!
for(i in 1:length(text_relation))
{
  temp_sentence_fenci=text_relation[[i]]
  temp_words=temp_sentence_fenci[temp_sentence_fenci %in% fenci_unique]
  if(length(temp_words>0))
  {
    for(x in 1:length(temp_words))
    {
      for(y in 1:length(temp_words))#####确定是单向还是双向的操作区域
      {
        data_matrix[temp_words[x],temp_words[y]]=data_matrix[temp_words[x],temp_words[y]]+1
      }
    }
  }
  print(paste(i,"/",length(text_relation)))
}
print("请稍等，写入文件中……——the little poor creature is weaving the words")
for(i in 1:ncol(data_matrix)){data_matrix[i,i]=0}
#form relation data.frame
source=rep(fenci_unique,length(fenci_unique))
target=c(0);for(i in 1:length(fenci_unique)){target=c(target,rep(fenci_unique[i],length(fenci_unique)))};target=target[-1]
b=as.vector(data_matrix)
weight=b
empty_df=cbind(source,target,weight)
empty_df[,3]=as.numeric(empty_df[,3])
all_relation=as.data.frame(empty_df)
#cof filter
b=as.numeric(b)
df_5=all_relation[b>5,]
df_10=all_relation[b>10,]
df_50=all_relation[b>50,]
df_100=all_relation[b>100,]
df_200=all_relation[b>200,]
write.csv(df_5,"edge_data_cof5.csv")
write.csv(df_10,"edge_data_cof10.csv")
write.csv(df_50,"edge_data_cof50.csv")
write.csv(df_100,"edge_data_cof100.csv")
write.csv(df_200,"edge_data_cof200.csv")
##
node_data=as.data.frame(cbind(as.character(fenci_unique),as.character(fenci_unique)));colnames(node_data)=c("id","label")
write.csv(node_data,"node_data.csv")
print("词共现网络搭建完成并已写出，网络数据见工作空间")
}
