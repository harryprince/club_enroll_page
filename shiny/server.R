require(shiny)
require(shinythemes)
require(shinysky)
require(knitr)
require(shinyAce)

## Utils may be used
helpTip <- function(cont, ...) {
    a(class = 'fa fa-question-circle help-tool-tip', `data-toggle` = 'tooltip', `data-html` = T,
      `data-title` = span(cont), ...)
}
helpPop <- function(cont, ti = NULL, ...) {
    a(class = 'fa fa-question-circle help-tool-tip', `data-toggle` = 'popover', `data-html` = T,
      `data-trigger` = 'hover', `data-title` = ti, `data-placement` = 'top', 
      `data-content` = span(cont), ...)
}

shinyServer(function(input, output, session) {
    ## Generate Recognition ID
    used_recog <- readLines('data/used_recog_id')
    recog <- paste(c(LETTERS[sample(1:26, 3)], sample(1:9, 3)), collapse = '')
    while (recog %in% used_recog) {
        recog <- paste(c(LETTERS[sample(1:26, 3)], sample(1:9, 3)), collapse = '')
    }
    
    ## Initialize Page Query
    pg <- reactiveValues(visit = 'home')
    
    ## Work With Captchas
    generateCAPTCHA <- function(fn) {
        src = sample(c(LETTERS, rep(1:9, 3)), 5)
        png(fn, width = 136, height = 34, pointsize = 34)
        par(mar = c(0, 0, 0, 0))
        plot(c(0, 5), c(0, 1), type = 'n', frame = F, axes = F)
        points(runif(10, 0, 5), runif(10, 0, 1), col = 'grey50', cex = runif(10, 0.5, 3))
        for (i in 1:5) {
            text(i - 0.5 + runif(1, -0.2, 0.2), 0.5 + runif(1, -0.2, 0.2), src[i],
                 cex = runif(0.8, 1), srt = runif(1, -20, 20),
                 col = paste0('grey', ceiling(runif(1, 0, 50))))
            abline(runif(1, -1, 1), runif(1, 0, 1), col = 'grey50')
        }
        dev.off()
        paste(src, collapse = '')
    }
    refreshCaptcha <- function() {
        f <- tempfile('captcha', 'captcha')
        pg$captcha <- list(ans = generateCAPTCHA(f), uri = image_uri(f))
        unlink(f)
    }
    output$captchaPanel <- renderUI({
        list(
            div(class = 'col-md-6 col-xs-12', textInput('userCaptcha', NULL, width = '100%')),
            div(class = 'col-md-3 col-xs-6', tags$img(class = 'img-responsive', src = pg$captcha$uri)),
            div(class = 'col-md-3 col-xs-6', actionButton('changeCaptchaAction', 'Change CAPTCHA', block = T))
        )
    })
    
    ## Page Rendering Functions
    getHomePage <- function() {
        list(
            div(class = 'jumbotron',
                h1('浙江大学', br(), 'Fantasy 动漫社'),
                h2('2016 春季纳新线上报名系统 beta-0.1', br(), tags$small('实际上没有在储存数据的测试版')),
                p('这里需要一些宣传语，每次进入页面随机显示一条'),
                br(), hr(), br(),
                actionButton('registNew', '☆ﾐ(o*･ω･)ﾉｲﾏﾀﾞ  加入我们！', styleclass = 'primary', block = T),
                actionButton('upgradeToWorkMan', '___〆(・∀・)  老社员报名部员入口', block = T)
            ),
            tags$script(type = 'text/javascript', src = 'tooltip-regist.js')
        )
    }
    getRegistNewPage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('基本社员报名'),
                        tags$small('报名加入F社基本社员，加入F社大家庭，结交志同道合的好友，参加F社组织的各种活动。如果愿意，你还可以继续报名成为部员，以组织者或创作者的身份参加到F社的各种活动中来！填写下面的信息，提交并按照提示缴纳会费后，请耐心等待F社的会员卡投递到你的寝室邮箱吧～'))
                ),
                div(class = 'panel-body',
                    div(class = 'row',
                        div(class = 'col-md-6',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',div(class = 'panel-tile', h4('个人信息'))),
                                div(class = 'panel-body', style = 'min-height:345px',
                                    textInput('registNewTruName', '真实姓名', width = '100%'),
                                    textInput('registNewNickName', list('昵称／ID', helpTip('在漫社的大家庭中互相称呼的名字，请三思后再填写哦！')), width = '100%'),
                                    selectInput('registNewGender', '性别', choices = c('男', '女', '其他', '-'), selected = '-', width = '100%'),
                                    dateInput('registNewBirthday', '生日', value = Sys.Date(), width = '100%', language = 'zh-CN')
                                )
                            )
                        ),
                        div(class = 'col-md-6',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',
                                    div(class = 'panel-tile', h4('联系方式'))),
                                div(class = 'panel-body', style = 'min-height:345px',
                                    textInput('registNewPhLo', '手机长号', width = '100%'),
                                    textInput('registNewPhSh', list('手机短号', helpTip('若无请留空')), width = '100%'),
                                    textInput('registNewQQ', 'QQ', width = '100%'),
                                    textInput('registNewDorm', '寝室号', '例：蓝4-1000', width = '100%')
                                )
                            )
                        ),
                        div(class = 'col-md-12',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',
                                    div(class = 'panel-tile', h4('自我介绍／Free Talk'),
                                        tags$small('你有什么特殊技能，你喜欢的动漫作品是什么，……', br(),
                                                   '在进入F社大家庭之前，请用几句话介绍一下自己吧 ＿〆(´∀｀●)~',
                                                   '(还可以输入 ', span(id = 'registNewIntroWC', 200),'/200 个字)'))),
                                div(class = 'panel-body',
                                    div(class = 'form-group shiny-input-container', style = 'width:100%;margin-bottom:0',
                                        tags$textarea(id = 'registNewIntro', class = 'form-control', rows = '4', style = 'resize: none', maxlength = 200, onkeyup = 'textAreaWC(this, \'registNewIntroWC\')'))
                                )
                            )
                        )
                    )
                )
            ),
            div(class = 'panel panel-primary',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('申请成为部员'),
                        tags$small('F社下设「COS部」「Drama部」「宅舞部」「创作部」以及新成立的「事务部」等部门，每个部门下又设有不同的小组，以不同的主题进行着更加深层次的活动，为F社的个大活动提供直接支持。'))
                ),
                div(class = 'panel-body',
                    includeHTML('sub_intro.html'),
                    div(class = 'panel panel-info',
                        div(class = 'panel-heading', style = 'cursor: pointer', `data-toggle` = 'collapse', `data-target` = '#beWorkManList',
                            div(class = 'panel-title', tags$small('我要报名成为部员，打开新世界的大门！＿〆(-ε･｀)ﾉ^☆'))),
                        div(class = 'panel-body collapse', id = 'beWorkManList',
                            selectInput('registWorkManWhat', '选择你要报名的部门／小组', width = '100%',
                                        choices = c('COS部 —— Coser组' = 'COS1', 'COS部 —— 妆效组' = 'COS2', 'COS部 —— 摄影组' = 'COS3',
                                                    'Drama部 —— CV组' = 'DRA1', 'Drama部 —— 后期组' = 'DRA2',
                                                    '宅舞部 —— Lovelive舞团' = 'DAN1', '宅舞部 —— 男子天团' = 'DAN2', '宅舞部 —— 偶像组' = 'DAN3', '宅舞部 —— 帅气组' = 'DAN4',
                                                    '创作部 —— 画手组' = 'ORI1', '创作部 —— 写手组' = 'ORI2', '创作部 —— MAD组' = 'ORI3',
                                                    '事务部 —— 活动与新闻方向' = 'WOR1', '事务部 —— 线上平台方向' = 'WOR2', '事务部 —— 实体宣传品方向' = 'WOR3', '事务部 —— 文件档案管理方向' = 'WOR4'),
                                        multiple = T),
                            div(class = 'form-group shiny-input-container', style = 'width:100%',
                                tags$label(`for` = 'registWorkManReason', '你选择报名这个／些部门小组的原因是？(', span(id = 'registWorkManReasonWC', 200), '/200）'),
                                tags$textarea(id = 'registWorkManReason', class = 'form-control', rows = '3', style = 'resize: none', maxlength = 200, onkeyup = 'textAreaWC(this, \'registWorkManReasonWC\')')),
                            div(class = 'form-group shiny-input-container', style = 'width:100%',
                                tags$label(`for` = 'registWorkManAbility', '你觉的你的哪些能力或特长可以为你所报名的部门贡献力量？(', span(id = 'registWorkManAbilityWC', 200), '/200）'),
                                tags$textarea(id = 'registWorkManAbility', class = 'form-control', rows = '3', style = 'resize: none', maxlength = 200, onkeyup = 'textAreaWC(this, \'registWorkManAbilityWC\')')),
                            div(class = 'form-group shiny-input-container', style = 'width:100%;margin-bottom:0',
                                tags$label(`for` = 'registWorkManOther', '除此之外你还有什么特长爱好呢，还有什么别的社团的相关经验呢？(', span(id = 'registWorkManAbilityWC', 200), '/200）'),
                                tags$textarea(id = 'registWorkManOther', class = 'form-control', rows = '3', style = 'resize: none', maxlength = 200, onkeyup = 'textAreaWC(this, \'registWorkManAbilityWC\')'))
                        )
                    )
                )
            ),
            uiOutput('captchaPanel', class = 'row'),
            uiOutput('addNewWrong'),
            actionButton('registSubmitAction', 'ᕕ(　 ᐛ )ᕗ   提交信息', styleclass = 'primary', block = T),
            actionButton('backToHomeAction', 'ヽ(　･∀･)ﾉ  返回首页 ', block = T),
            tags$script(type = 'text/javascript', src = 'tooltip-regist.js')
        )
    }
    getRegistNewDonePage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading', div(class = 'panel-title', h4('服务器娘已经收到你的信息啦 ԅ(´ڡ`ԅ)'))),
                div(class = 'panel-body',
                    p(tags$strong('请务必复制并记下下面的识别码')),
                    div(class = 'form-group', tags$input(id = 'recogID', type = 'text', class = 'form-control', style = 'font-family:monospace;cursor:default', value = recog, readonly = T)),
                    tags$script('$("#recogID").focus();$("#recogID").select();'),
                    p('并将', tags$strong('15 元会费'), '通过支付宝转账到下面的账户，同时在', tags$strong('备注信息'), '中附上上面的代码'),
                    p('此处应有糖一小盆宇的支付宝二维码')
                )
            ),
            div(class = 'panel panel-info',
                div(class = 'panel-heading', div(class = 'panel-title', h4('可以在这里找到小伙伴们哟！(｡･ω･｡)ﾉ♡'))),
                div(class = 'panel-body',
                    div(class = 'row',
                        div(class = 'col-md-4 col-xs-12', tags$img(src = 'img/qr_qq.png', class = 'img-thumbnail img-responsive')),
                        div(class = 'col-md-4 col-xs-12', tags$img(src = 'img/qr_wechat.png', class = 'img-thumbnail img-responsive')),
                        div(class = 'col-md-4 col-xs-12', tags$img(src = 'img/qr_weibo.png', class = 'img-thumbnail img-responsive'))
                    )
                )
            ),
            actionButton('backToHomeAction', 'ヽ(　･∀･)ﾉ  返回首页 ', block = T, styleclass = 'primary')
        )
    }
    
    ## Observers
    observeEvent(pg$visit, {
        if (pg$visit == 'home') {
            output$page <- renderUI(getHomePage())
        } else if (pg$visit == 'rgnw') {
            refreshCaptcha()
            output$page <- renderUI(getRegistNewPage())
        } else if (pg$visit == 'upgd') {
            
        } else if (pg$visit == 'rgnwDone') {
            output$page <- renderUI(getRegistNewDonePage())
        }
    })
    observeEvent(input$registNew, {
        pg$visit <- 'rgnw'
    })
    observeEvent(input$registSubmitAction, {
        if (toupper(input$userCaptcha) != pg$captcha$ans) {
            output$addNewWrong <- renderUI(helpText('验证码错误啦！'))
            refreshCaptcha()
        } else {
            ## Save the data
            pg$visit <- 'rgnwDone'
        }
    })
    observeEvent(input$backToHomeAction, {
        pg$visit <- 'home'
    })
    observeEvent(input$changeCaptchaAction, {
        refreshCaptcha()
    })
})