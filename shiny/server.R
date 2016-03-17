require(shiny)
require(shinythemes)
require(shinysky)
require(knitr)
require(RJSONIO)

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
    getRecog <- function() {
        recog <- paste(c(LETTERS[sample(1:26, 3)], sample(1:9, 3)), collapse = '')
        while (file.exists(paste0('data/RECOG/', recog))) {
            recog <- paste(c(LETTERS[sample(1:26, 3)], sample(1:9, 3)), collapse = '')
        }
        return(recog)
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
            div(class = 'col-md-3 col-xs-6', actionButton('changeCaptchaAction', '更换验证码', block = T))
        )
    })
    
    ## Page Rendering Functions
    getHomePage <- function() {
        list(
            div(class = 'jumbotron',
                h1('浙江大学', br(), 'Fantasy 动漫社'),
                h2('2016 春季纳新线上报名系统 alpha-1.0'), br(),
                br(), hr(), br(),
                actionButton('registNew', '☆ﾐ(o*･ω･)ﾉｲﾏﾀﾞ  加入我们！', styleclass = 'primary', block = T),
                actionButton('upgradeToWorkMan', '___〆(・∀・)  老社员报名部员入口', block = T),
                actionButton('checkProgress', '(シ. _ .)シ检查付款确认情况', block = T)
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
                                    textInput('registNewTrueName', '真实姓名＊', width = '100%'),
                                    textInput('registNewNickName', list('昵称／ID ＊', helpTip('在漫社的大家庭中互相称呼的名字，请三思后再填写哦！')), width = '100%'),
                                    selectInput('registNewGender', '性别', choices = c('男', '女', '其他', '-'), selected = '-', width = '100%'),
                                    dateInput('registNewBirthday', '生日', value = Sys.Date() + (as.Date('2000-1-1') - as.Date('2017-1-1')), width = '100%', language = 'zh-CN', startview = 'decade')
                                )
                            )
                        ),
                        div(class = 'col-md-6',
                            div(class = 'panel panel-primary',
                                div(class = 'panel-heading',
                                    div(class = 'panel-tile', h4('联系方式'))),
                                div(class = 'panel-body', style = 'min-height:345px',
                                    textInput('registNewPhLo', '手机长号＊', width = '100%'),
                                    textInput('registNewPhSh', list('手机短号', helpTip('若无请留空')), width = '100%'),
                                    textInput('registNewQQ', 'QQ', width = '100%'),
                                    textInput('registNewDorm', list('寝室号＊', helpTip('请务必正确填写，保证会员卡投递无误～')), '例：蓝4-1000', width = '100%')
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
                            div(class = 'panel-title', tags$small('我要报名成为部员＿〆(-ε･｀)ﾉ^☆，点击这里打开新世界的大门！'))),
                        div(class = 'panel-body collapse', id = 'beWorkManList',
                            selectInput('registWorkManWhat', '选择你要报名的部门／小组', width = '100%',
                                        choices = c('COS部 —— Coser组' = 'COS1', 'COS部 —— 妆效组' = 'COS2', 'COS部 —— 摄影组' = 'COS3',
                                                    'Drama部 —— CV组' = 'DRA1', 'Drama部 —— 后期组' = 'DRA2',
                                                    '宅舞部 —— Lovelive舞团' = 'DAN1', '宅舞部 —— 男子天团' = 'DAN2', '宅舞部 —— 偶像组' = 'DAN3', '宅舞部 —— 帅气组' = 'DAN4',
                                                    '创作部 —— 画手组' = 'ORI1', '创作部 —— 写手组' = 'ORI2', '创作部 —— MAD组' = 'ORI3',
                                                    '事务部 —— 活动与新闻方向' = 'WOR1', '事务部 —— 线上平台方向' = 'WOR2', '事务部 —— 实体宣传品方向' = 'WOR3', '事务部 —— 文件档案管理方向' = 'WOR4'),
                                        multiple = T),
                            selectInput('registWorkManIntv', '在以下时段中选择可以参加面试的时间', width = '100%',
                                        choices = c('3月26日（六）13:30-15:30' = 1, '3月26日（六）15:30-17:30' = 2,
                                                    '以上时间均不可，希望另安排时间' = 3),
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
                    div(class = 'form-group', tags$input(id = 'recogID', type = 'text', class = 'form-control', style = 'font-family:monospace;cursor:default', value = isolate(pg$recog), readonly = T)),
                    tags$script('$("#recogID").focus();$("#recogID").select();'),
                    p('并将', tags$strong('15 元会费'), '通过支付宝转账到下面的账户，同时在', tags$strong('备注信息'), '中附上上面的代码。', br(),
                      '付款结束后你还可以使用这个代码来查询转账确认情况，因为转账需要手工确认，请耐心等待哦！'),
                    div(class = 'panel panel-primary', 
                        div(class = 'panel-body', tags$img(src = 'img/qr_pay.png', class = 'img-circle img-responsive', align = 'middle', style = 'max-width:300px;margin:auto')),
                        div(class = 'panel-footer', span('支付宝账户：', tags$strong('shsj_zju@126.com')))
                    )
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
    getUpgradePage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading',
                    div(class = 'panel-title', h4('申请成为部员'),
                        tags$small('F社下设「COS部」「Drama部」「宅舞部」「创作部」以及新成立的「事务部」等部门，每个部门下又设有不同的小组，以不同的主题进行着更加深层次的活动，为F社的个大活动提供直接支持。'))
                ),
                div(class = 'panel-body',
                    div(class = 'row',
                        div(class = 'col-md-6 col-xs-12', textInput('oldRecog', '会员卡号＊', width = '100%')),
                        div(class = 'col-md-6 col-xs-12', textInput('oldNickName', '昵称／ID＊', width = '100%'))
                    ),
                    includeHTML('sub_intro.html'),
                    div(class = 'panel panel-info',
                        div(class = 'panel-heading', style = 'cursor: pointer',
                            div(class = 'panel-title', tags$small('打开新世界的大门！＿〆(-ε･｀)ﾉ^☆'))),
                        div(class = 'panel-body', id = 'beWorkManList',
                            selectInput('registWorkManWhat', '选择你要报名的部门／小组', width = '100%',
                                        choices = c('COS部 —— Coser组' = 'COS1', 'COS部 —— 妆效组' = 'COS2', 'COS部 —— 摄影组' = 'COS3',
                                                    'Drama部 —— CV组' = 'DRA1', 'Drama部 —— 后期组' = 'DRA2',
                                                    '宅舞部 —— Lovelive舞团' = 'DAN1', '宅舞部 —— 男子天团' = 'DAN2', '宅舞部 —— 偶像组' = 'DAN3', '宅舞部 —— 帅气组' = 'DAN4',
                                                    '创作部 —— 画手组' = 'ORI1', '创作部 —— 写手组' = 'ORI2', '创作部 —— MAD组' = 'ORI3',
                                                    '事务部 —— 活动与新闻方向' = 'WOR1', '事务部 —— 线上平台方向' = 'WOR2', '事务部 —— 实体宣传品方向' = 'WOR3', '事务部 —— 文件档案管理方向' = 'WOR4'),
                                        multiple = T),
                            selectInput('registWorkManIntv', '在以下时段中选择可以参加面试的时间', width = '100%',
                                        choices = c('3月26日（六）13:30-15:30' = 1, '3月26日（六）15:30-17:30' = 2,
                                                    '以上时间均不可，希望另安排时间' = 3),
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
            actionButton('upgradeSubmitAction', 'ᕕ(　 ᐛ )ᕗ   提交信息', styleclass = 'primary', block = T),
            actionButton('backToHomeAction', 'ヽ(　･∀･)ﾉ  返回首页 ', block = T),
            tags$script(type = 'text/javascript', src = 'tooltip-regist.js')
        )
    }
    getUpgradeDonePage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading', div(class = 'panel-title', h4('服务器娘已经收到你的信息啦 ԅ(´ڡ`ԅ)'))),
                div(class = 'panel-body',
                helpText('可以在这里找到小伙伴们哟！(｡･ω･｡)ﾉ♡'),
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
    getCheckProgressPage <- function() {
        list(
            div(class = 'panel panel-primary',
                div(class = 'panel-heading', div(class = 'panel-title', h4('收款确认查询(シ. _ .)シ'))),
                div(class = 'panel-body',
                    div(class = 'row',
                        div(class = 'col-xs-9', textInput('checkRecog', NULL, '输入查询的识别码', '100%')),
                        div(class = 'col-xs-3', actionButton('checkProgressAction', '确认', styleclass = 'primary', block = T))
                    ),
                    uiOutput('checkRes')
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
            refreshCaptcha()
            output$page <- renderUI(getUpgradePage())
        } else if (pg$visit == 'rgnwDone') {
            output$page <- renderUI(getRegistNewDonePage())
        } else if (pg$visit == 'upgdDone') {
            output$page <- renderUI(getUpgradeDonePage())
        } else if (pg$visit == 'check') {
            output$page <- renderUI(getCheckProgressPage())
        }
    })
    observeEvent(input$registNew, {
        pg$visit <- 'rgnw'
    })
    observeEvent(input$upgradeToWorkMan, {
        pg$visit <- 'upgd'
    })
    observeEvent(input$checkProgress, {
        pg$visit <- 'check'
    })
    observeEvent(input$registSubmitAction, {
        if (toupper(input$userCaptcha) != pg$captcha$ans) {
            output$addNewWrong <- renderUI(helpText('验证码错误啦！'))
            refreshCaptcha()
        } else {
            form <- list(
                tn = input$registNewTrueName, nn = input$registNewNickName, gd = input$registNewGender, bd = input$registNewBirthday,
                pl = input$registNewPhLo, ps = input$registNewPhSh, qq = input$registNewQQ, dm = input$registNewDorm,
                ft = input$registNewIntro,
                wo = input$registWorkManWhat, ti = input$registWorkManIntv,
                q1 = input$registWorkManReason, q2 = input$registWorkManAbility, q3 = input$registWorkManOther
            )
            if (form$tn == '' || nchar(form$tn) > 10) {
                output$addNewWrong <- renderUI(helpText('请正确的填写真实姓名！（1-10 字符）'))
            } else if (form$nn == '' || nchar(form$nn) > 10) {
                output$addNewWrong <- renderUI(helpText('请正确的填写昵称！（1-10 字符）'))
            } else if (grepl('[^0-9]', form$pl) || nchar(form$pl) != 11) {
                output$addNewWrong <- renderUI(helpText('请正确的填写手机长号！（11 位数字）'))
            } else if (form$ps != '' && !(grepl('[0-9]', form$ps) && nchar(form$ps) == 6)) {
                output$addNewWrong <- renderUI(helpText('请正确的填写手机短号！（6 位数字）'))
            } else {
                pg$recog <- getRecog()
                ## Regist The regoc ID
                file.create(paste0('data/RECOG/', pg$recog))
                ## If the user regist for workman, regist the info
                if (!is.null(form$wo)) {
                    for (wo in form$wo) {
                        file.create(paste0('data/WORKMAN/', wo, '/', pg$recog))
                    }
                    if (is.null(form$ti)) {
                        file.create(paste0('data/INTERVIEW/3/', pg$recog))
                    } else {
                        for (ti in form$ti) {
                            file.create(paste0('data/INTERVIEW/', ti, '/', pg$recog))
                        }
                    }
                }
                ## Store the form as json
                writeLines(toJSON(form), paste0('data/form/', pg$recog))
                output$addNewWrong <- renderUI(NULL)
                pg$visit <- 'rgnwDone'
            }
        }
    })
    observeEvent(input$upgradeSubmitAction, {
        if (toupper(input$userCaptcha) != pg$captcha$ans) {
            output$addNewWrong <- renderUI(helpText('验证码错误啦！'))
            refreshCaptcha()
        } else {
            form <- list(
                no = input$oldRecog, nn = input$oldNickName,
                wo = input$registWorkManWhat, ti = input$registWorkManIntv,
                q1 = input$registWorkManReason, q2 = input$registWorkManAbility, q3 = input$registWorkManOther
            )
            if (grepl('[^0-9]', form$no) || nchar(form$no) != 7) {
                output$addNewWrong <- renderUI(helpText('请正确的填写会员卡号！（7 位数字）'))
            } else if (is.null(form$wo)) {
                output$addNewWrong <- renderUI(helpText('请至少选择一个你希望报名的部门／小组'))
            } else {
                if (file.exists(paste0('data/RECOG/', form$no))) {
                    output$addNewWrong <- renderUI(helpText('您输入的会员卡号貌似已经被使用啦！联系梨梓（QQ：597022601）查看后台数据吧'))
                } else if (form$nn == '' || nchar(form$nn) > 10) {
                    output$addNewWrong <- renderUI(helpText('请正确的填写昵称！（1-10 字符）'))
                } else {
                    pg$recog <- form$no
                    ## Regist The regoc ID
                    file.create(paste0('data/RECOG/', pg$recog))
                    for (wo in form$wo) {
                        file.create(paste0('data/WORKMAN/', wo, '/', pg$recog))
                    }
                    if (is.null(form$ti)) {
                        file.create(paste0('data/INTERVIEW/3/', pg$recog))
                    } else {
                        for (ti in form$ti) {
                            file.create(paste0('data/INTERVIEW/', ti, '/', pg$recog))
                        }
                    }
                    ## Store the form as json
                    writeLines(toJSON(form), paste0('data/form/', pg$recog))
                    output$addNewWrong <- renderUI(NULL)
                    pg$visit <- 'upgdDone'
                }
            }
        }
    })
    observeEvent(input$checkProgressAction, {
        recog <- input$checkRecog
        if (nchar(recog) != 6) {
            output$checkRes <- renderUI(helpText('请输入正确的识别码（3 位字母 3 位数字）'))
        } else if (!file.exists(paste0('data/RECOG/', recog))) {
            output$checkRes <- renderUI(helpText('没有找到您输入的识别码'))
        } else if (!file.exists(paste0('data/CHECK/', recog))) {
            output$checkRes <- renderUI(helpText('暂时还没有确认付款，因为确认工作需手工完成，请您耐心等待'))
        } else {
            output$checkRes <- renderUI(helpText('已经确认收到您的付款！请等待我们向您投递会员卡！'))
        }
    })
    observeEvent(input$backToHomeAction, {
        pg$visit <- 'home'
    })
    observeEvent(input$changeCaptchaAction, {
        refreshCaptcha()
    })
})