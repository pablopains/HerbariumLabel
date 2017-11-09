########################################################################
# Pacotes Necessários
########################################################################
#rm(list = ls())
#memory.limit(size = 4000) 

if(!require(pacman)) install.packages("pacman")
pacman::p_load(rmarkdown, knitr, data.table)

########################################################################
# Gera Fichas
########################################################################
gerafichas<-function(coletas,qtdefichaspagina=6,qtdemaxpaginasjuncao=15,pastafichas)
{
  nficha<-0
  nfile_Rdm<-0
  fichas=as.list({})
  rmd <- list.files(pastafichas,pattern = '*.Rmd', include.dirs = FALSE)
  if(length(rmd)>0){file.remove(paste0(pastafichas,'/',rmd))}
  
  for (i in 1:NROW(coletas)){
    #dup=ifelse(is.na(coletas$dups[i]==''),1,as.integer(coletas$dups[i])+1) 
    # quand dup = numero de fichas
    
    # +1
    dup=ifelse(is.na(coletas$duplicatesNumber[i]),1,as.integer(coletas$duplicatesNumber[i])) 
    dup=ifelse(is.na(coletas$duplicatesNumber[i]==''),1,as.integer(coletas$duplicatesNumber[i])) 
    dup=ifelse(as.integer(coletas$duplicatesNumber[i]==0),1,as.integer(coletas$duplicatesNumber[i])) 
    
    dup=ifelse(is.na(dup),1,dup) 
    
    #aqui dup+1
    print(dup)
    for (d in 1:dup){
      nfile_Rdm=nfile_Rdm+1
      nficha=nficha+1
      if(nficha==1){fichas=append(fichas,list(f1=carregaficha(coletas[i,])))}
      if(nficha==2){fichas=append(fichas,list(f2=carregaficha(coletas[i,])))} 
      if(nficha==3){fichas=append(fichas,list(f3=carregaficha(coletas[i,])))} 
      if(nficha==4){fichas=append(fichas,list(f4=carregaficha(coletas[i,])))} 
      if(nficha==5){fichas=append(fichas,list(f5=carregaficha(coletas[i,])))} 
      if(nficha==6){fichas=append(fichas,list(f6=carregaficha(coletas[i,])))} 
      
      if(nficha==qtdefichaspagina|(i==NROW(coletas)&d==dup)){
        file_Rdm<-paste0('FichaHRCB_',qtdefichaspagina,'.Rmd')
        #file_Rdm<-paste0('FichaHRCB_1C','.Rmd')
        file_outout<-paste0(nfile_Rdm,'.Rmd')
        render(file_Rdm,
               output_file=file_outout,
               output_dir=pastafichas,
               intermediates_dir=paste0(pastafichas,'/tmp'))
        fichas=as.list({})
        nficha=0}
    }
  }   
  agrupafichas(qtdefichaspagina,pastafichas,qtdemaxpaginasjuncao)
}

########################################################################
# Agrupa Fichas: em um único arquivo ou varios com 100 
########################################################################
agrupafichas<-function(qtdefichaspagina=6,pastafichas,qtdemaxpaginasjuncao=15)
{
  nfile_Out=1
  rmd <- list.files(pastafichas,pattern = '*.Rmd', recursive = T, include.dirs = F)
  
  rmd.tmp<-{} #
  
  txt.Rmd=NULL
  for(p in 1:length(rmd)){
    nfile_Rdm=p*qtdefichaspagina
    file_Rmd<-paste0(pastafichas,'/',nfile_Rdm,'.Rmd')
    
    #comparar com rmd e processar a difirença
    rmd.tmp[p]<-paste0(nfile_Rdm,'.Rmd')
    
    
    #####file_Rmd<-paste0(pastafichas,'/',rmd[p])
    if (file.exists(file_Rmd)){
      txt <- paste0("```{r child = '", file_Rmd, "'}\n```\n")
      txt.Rmd=ifelse(!is.null(txt.Rmd),paste0(txt.Rmd,"\n",txt),txt)}
    
    if(nfile_Rdm>=(qtdemaxpaginasjuncao*qtdefichaspagina*nfile_Out)){
      file.juncao=paste0(pastafichas,'/','Juncao.Rmd')
      write(txt.Rmd, file=file.juncao)
      
      output_file_html=paste0('FichaHerbario_',nfile_Out,'.html')
      render(file.juncao,
             output_file=output_file_html,
             output_dir='fichas',
             intermediates_dir='fichas/tmp',
             encoding="utf-8")
      file.remove(file.juncao)
      file.copy(paste0(pastafichas,'/',output_file_html),paste0(pastafichas,'/',gsub('.html','.doc',output_file_html)),overwrite=TRUE)       
      txt.Rmd=NULL
      nfile_Out=nfile_Out+1
    }
  }  
  
  ###processar a diferença
  if (length(rmd.tmp[rmd.tmp%in%rmd])>0){
    file_Rmd<-paste0(pastafichas,'/',rmd[!rmd%in%rmd.tmp])
    if (file.exists(file_Rmd)){
      txt <- paste0("```{r child = '", file_Rmd, "'}\n```\n")
      txt.Rmd=ifelse(!is.null(txt.Rmd),paste0(txt.Rmd,"\n",txt),txt)}}
  #txt.Rmd <- paste0("```{r child = '", file_Rmd, "'}\n```\n")}}
  
  # aqui imprimir o restante
  if(!is.null(txt.Rmd)){
    file.juncao=paste0(pastafichas,'/','Juncao.Rmd')
    write(txt.Rmd, file=file.juncao)
    
    output_file_html=paste0('FichaHerbario_',nfile_Out,'.html')
    render(file.juncao,
           output_file=output_file_html,
           output_dir='fichas',
           intermediates_dir='fichas/tmp',
           encoding="utf-8")
    
    file.remove(file.juncao)
    
    file.copy(paste0(pastafichas,'/',output_file_html),paste0(pastafichas,'/',gsub('.html','.doc',output_file_html)),overwrite=TRUE)       
  }
  rmd <- list.files(pastafichas,pattern = '*.Rmd', include.dirs = FALSE)
  if(length(rmd)>0){file.remove(paste0(pastafichas,'/',rmd))}
} 

agrupafichas_tudo<-function(qtdefichaspagina=6,pastafichas)
{
  rmd <- list.files(pastafichas,pattern = '*.Rmd', recursive = T, include.dirs = F)
  
  txt.Rmd=NULL
  for(p in 1:length(rmd)){
    nfile_Rdm=p*qtdefichaspagina
    
    file_Rmd<-paste0(pastafichas,'/',nfile_Rdm,'.Rmd')
    if (file.exists(file_Rmd)){
      txt <- paste0("```{r child = '", file_Rmd, "'}\n```\n")
      txt.Rmd=ifelse(!is.null(txt.Rmd),paste0(txt.Rmd,"\n",txt),txt)}
  }  
  file.juncao=paste0(pastafichas,'/','Juncao.Rmd')
  #write.csv(txt.Rmd, file=file.juncao, row.names=FALSE) #https://egret.psychol.cam.ac.uk/statistics/R/savingloading.html
  write(txt.Rmd, file=file.juncao)
  render(file.juncao,
         output_file='Fichas-Imprimir.html',
         output_dir='fichas',
         intermediates_dir='fichas/tmp',
         encoding="utf-8")
  
  file.remove(file.juncao)
  
  
  rmd <- list.files(pastafichas,pattern = '*.Rmd', include.dirs = FALSE)
  if(length(rmd)>0){file.remove(paste0(pastafichas,'/',rmd))}
  file.copy(paste0(pastafichas,'/','Fichas-Imprimir.html'),paste0(pastafichas,'/','Fichas-Imprimir.doc'),overwrite=TRUE)}  

########################################################################
########################################################################
# carrega registro para ficha
########################################################################
#### ajustar duplicatasherbarios<-'R,FSP,BHCB,HFEFS'
carregaficha<-function(coletas)
{
  return(data.frame(
    
    logoherbario<-LogoHerbario,
    siglaherbario<-coletas$collectionCode,
    nomeherbario<-NomeHerbario,
    instituicao<-Instituicao,
    projeto<-Projeto,
    
    numerotombo<-limpaNA(coletas$catalogNumber),
    familia<-ifelse(nchar(limpaNA(coletas$family))>0,limpaNA(coletas$family),'.'),
    genero<-ifelse(nchar(limpaNA(coletas$genus))>0,limpaNA(coletas$genus),'.'),
    epiteto_especifico<-limpaNA(coletas$specificEpithet),
    epiteto_infraespecifico<-limpaNA(coletas$infraspecificEpithet),
    cf<-limpaNA(coletas$identificationQualifier),
    nomecientifico<-limpaNA(coletas$scientificName),
    autor<-limpaNA(coletas$scientificNameAuthorship),
    
    especieformatada<-formataespecie(nomecientifico,genero,epiteto_especifico,epiteto_infraespecifico,cf,autor),
    
    determinador<-ifelse(nchar(limpaNA(coletas$identifiedBy))>0,paste0('<sup><b>Det.: </b></sup>',limpaNA(coletas$identifiedBy)),'.'),
    #datadet<-limpaNA(paste0(coletas$dayidentified,'/',coletas$monthidentified,'/',coletas$yearidentified)),
    datadet<- montadata(limpaNA(coletas$dayIdentified),limpaNA(coletas$monthIdentified),limpaNA(coletas$yearIdentified)),
    pais<-limpaNA(coletas$country),
    municipio<-limpaNA(coletas$municipality),
    estado<-limpaNA(coletas$stateProvince),
    lat<-limpaNA(coletas$verbatimLatitude),
    long<-limpaNA(coletas$verbatimLongitude),
    alt<-limpaNA(coletas$elevation),
    localizacao<-limpaNA(paste0(pais,', ',municipio,', ',estado,'.',' ( ',lat,' S, ',long,' W, ',alt,' m )')),
    
    localidade<-limpaNA(limpaNA(coletas$locality)),
    
    datacoleta<-limpaNA(paste0(coletas$day,'/',coletas$month,'/',coletas$year)),
    
    numerocoleta<-limpaNA(coletas$recordNumber),
    coletorprincipal<-limpaNA(coletas$recordedBy),
    outroscoletores<-limpaNA(coletas$recordedByTeam),
    coletorformatado<-formatacoletor(coletorprincipal,outroscoletores,numerocoleta),
    
    #numeroduplicatas<-limpaNA(coletas$dups),
    # quand dup = numero de fichas
    numeroduplicatas<-as.character(as.integer(limpaNA(coletas$duplicatesNumber))-1),
    
    duplicatasherbarios<-coletas$duplicatesTo,
    
    notas<-limpaNA(paste0(coletas$lifeForm,separa(coletas$lifeFormComplement),separa(coletas$substrate),separa(coletas$habitat),separa(coletas$vegetation),separa(coletas$fieldNotes))),
    obs<-ObservacaoFicha))
}
########################################################################

########################################################################
# Pequenas funcçoes de formatção e limpeza de dados
########################################################################
formatacoletor<-function(coletor, equipe, numero)
{return(ifelse(equipe!='',
               paste0('<b>',coletor,', ',numero,'</b> & ' ,equipe),
               paste0('<b>',coletor,', ',numero,'</b>')))}

formataespecie<-function(nomecientifico,genero,epiteto_especifico,epiteto_infraespecifico,cf,autor)
{
  if(epiteto_infraespecifico==''){
    if(epiteto_especifico==''){autor<-''}
    especie<-paste0('<i>',genero,' </i>',separa(cf,' '),'<i>',epiteto_especifico,'</i>',separa(autor,' '))}
  
  if(epiteto_infraespecifico!='' & genero!='' & epiteto_especifico!=''){especie<-limpaNA(nomecientifico)}
  return(especie)
}

separa<-function(x,sep=',')
{return(ifelse(x!='',paste0(sep,' ',x),''))}

separadata<-function(x)
{return(ifelse(x!='',paste0(x,'/'),''))}

montadata<-function(dia='',mes='',ano='')
{return(paste0(ifelse(dia!='',paste0(dia,'/'),''),
               ifelse(mes!=''&ano!='',paste0(mes,'/',ano),''),
               ifelse(ano!=''&mes=='',ano,'')))} 
  
  
limpaNA<-function(x)
{
  if(is.na(x)){x<-''}
  x<-gsub('NA','',x)
  return(x)}
########################################################################

