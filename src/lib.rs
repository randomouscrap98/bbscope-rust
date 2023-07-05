use std::sync::Arc;

use regex::{Regex, Captures, Error};

// Carlos Sanchez - 2022-12-05
// - For SBS

//So:
//- early closures close all tags in the previous scope
//- ignore unmatched closing tags
//- close all unclosed tags at the end
//- block-level tags consume the first newline after the open and close tags

static AUTOLINKID: &str = "autolinker";
static CONSUMETEXTID : &str = "consumetext";
static NORMALTEXTID: &str = "normaltext";
static BRNEWLINEID: &str = "convertnewlinebr";
static NEWLINEID: &str = "newline";

const BASICTAGS: &[&str] = &[
    "b", "i", "sup", "sub", "u", "s", "list", r"\*", "url", "img"
];
const EXTENDEDTAGS: &[&str] = &[
    "h1", "h2", "h3", "anchor", "quote", "spoiler", "icode", "code", "youtube"
];

/// The type for your emit closure which will take the open tag capture, body, and close tag capture and 
/// output whatever you want. used with 
pub type EmitScope = Arc<dyn Fn(Option<Captures>, &str, Option<Captures>)->String + Send + Sync>; 
/// The type for your emit closure when you're doing basic regex replacement with [`MatchType::Simple`]
pub type EmitSimple = Arc<dyn Fn(Captures)->String + Send + Sync>; //Inefficient to use String but we have to in order to support replacements etc

/// Information about a scoped tag with open and close elements. This gives you the power to craft
/// many kinds of markup, not just bbcode, and have it understand scope
pub struct ScopeInfo {
    /// A list of [`MatchInfo`] ids for tags which are allowed to be parsed within the body of 
    /// this scope. If [`None`], any tag is allowed (default)
    pub only: Option<Vec<&'static str>>,
    /// Whether or not this scope automatically closes previous scopes of the same type. Generally
    /// this isn't how tags work: doing [b][b][b]EXTRA BOLD[/b][/b][/b] produces triple bold text.
    /// However, elements like [*] (list item) benefit from closing the previous scope if it's the
    /// same type
    pub double_closes: bool,
    /// The core of the parsing system: your provided closure which transforms the pre-parsed data
    /// given into HTML (or whatever)
    pub emit: EmitScope, 
}

impl Default for ScopeInfo {
    /// Create a default scope info with an emitter that only outputs the body of the tags
    /// (you probably don't want this!)
    fn default() -> Self {
        Self {
            only: None,
            double_closes: false,
            emit: Arc::new(|_o, b, _c| String::from(b))
        }
    }
}

impl ScopeInfo {
    /// Create a basic scope info using only an emiter (a highly common case, you probably want this)
    pub fn basic(emitter: EmitScope) -> Self {
        let mut result = Self::default();
        result.emit = emitter;
        result
    }
}

/// This defines how scopes and whole blocks of text move into the output. Basically: is your matcher
/// using scope or not? If using scope, you need to define an open and close [`MatchInfo`]. If not, 
/// just use the [`Simple`] option
pub enum MatchType { 
    /// A match type that requires no scoping rules: just a simple regex replacement (or whatever replacement you want)
    Simple(EmitSimple), 
    /// The match should expect an open tag, which increases scope and performs open scope rules
    Open(Arc<ScopeInfo>), 
    /// The match should expect a closing tag, which decreases scope and performs close scope rules
    Close
}

/// Definition for a block level matcher. Should always be readonly, it is just a definition. 
/// Not necessarily a scoped element, could define eating garbage, passing normal text through, etc.
/// It's all up to the 'match_type'
pub struct MatchInfo {
    /// A unique identifier for you to reference in things like [`ScopeInfo.only`]
    pub id: &'static str,
    /// The regex for this parse item. Most likely an open or close tag, but you can do anything you want
    pub regex : Regex,
    /// The type of match, indicates whether to open or close a scope (or perform no scoping)
    pub match_type: MatchType,
}

//For the following section, it is assumed the entire scoper and all elements within will have the same lifetime
//as the calling function, which also houses the input string.

/// A scope for bbcode tags. Scopes increase and decrease as tags are opened and closed. Scopes are placed on a stack
/// to aid with auto-closing tags
struct BBScope<'a> {
    /// Id of the [`MatchInfo`] which produced this scope. Used for tracking, double_closes detection, etc
    id: &'static str,
    /// The scope information from the [`MatchInfo`] which describes the rules of this current scope. Should
    /// always be a reference, as it's just informational
    info: Arc<ScopeInfo>,
    /// The regex capture for the open tag. We save this for later, when we finally emit the completed scope.
    /// Your [`EmitScope`] closure in [`ScopeInfo`] receives this as the first argument
    open_tag_capture: Option<Captures<'a>>,
    /// The currently retained body of this scope. If the scope is on top, this is where general text goes
    /// before the closing tag is detected. Your [`EmitScope`] closure in [`ScopeInfo`] receives this as the 
    /// second argument
    body: String, 
}

impl BBScope<'_> {
    /// Is the given element `info` (such as bold/italic/url) allowed to exist inside this scope
    fn is_allowed(&self, info: &MatchInfo) -> bool {
        if let Some(only) = &self.info.only {
            //A special case: your own closing tag is always allowed of course!
            if self.id == info.id && matches!(info.match_type, MatchType::Close) {
                return true;
            } 
            for &only_str in only.iter() {
                //If the only_str starts with attr:, we only allow this inner tag if the current scope
                //tag has an attr (attribute).
                if only_str.starts_with("attr:") {
                    let only_name = &only_str[5..];
                    if only_name == info.id {
                        if let Some(ref capture) = self.open_tag_capture {
                            if capture.name("attr").is_some() {
                                return true;
                            }
                        }
                    }
                }
                else if only_str == info.id {
                    return true;
                }
            }
            //Nothing was found, you're not allowed
            return false;
        }
        //If there's no only field, allow everything
        else {
            true
        }
    }
    /// Does the opener of this element with given id (such as bold/italic/etc) close this scope?
    fn closes(&self, id: &str) -> bool {
        self.id == id && self.info.double_closes
    }
    /// Consume the scope and emit the text. Only do this when you're the top scope on the stack!
    fn emit(self, close_captures: Option<Captures>) -> String {
        let emitter = &self.info.emit;
        emitter(self.open_tag_capture, &self.body, close_captures)
    }
}

/// A container with functions to help manage scopes. It doesn't understand what bbcode is or how the tags should
/// be formatted, it just handles pushing and popping scopes on the stack
struct BBScoper<'a> {
    scopes : Vec<BBScope<'a>>
}

/// Everything inside BBScoper expects to live as long as the object itself. So everything is 'a
impl<'a> BBScoper<'a> 
{
    /// Create a new scoper service with the starting scope already applied. The starting scope
    /// is the catch-all for the final string to output, it must exist
    fn new() -> Self { 
        Self { 
            scopes: vec![
                BBScope { 
                    id: "STARTING_SCOPE", 
                    info: Arc::new(ScopeInfo::default()) ,
                    open_tag_capture: None, 
                    body: String::new() 
                }
            ]
        }
    }

    /// Given the current state of our scopes, is the element with the given id (such as bold/italic/url)
    /// allowed to exist as the next element inside us right now? Basically: can the current scope accept this element
    fn is_allowed(&self, id: &MatchInfo) -> bool {
        self.scopes.last().unwrap().is_allowed(id)
    }

    /// Remove the top scope, emitting the output into the scope below it
    fn close_last(&mut self, close_tag: Option<Captures>) {
        if let Some(scope) = self.scopes.pop() {
            let body = scope.emit(close_tag); //this consumes the scope, which is fine
            self.add_text(&body);
        }
        else {
            panic!("BBScoper::close_last HOW DID THIS HAPPEN? There were scopes from .last but pop returned none!");
        }
    }

    /// Append a string to the current top scope, useful if you're just passing through normal user text
    fn add_text(&mut self, text: &str) {
        // I don't know how to do this the right way, I'm sorry
        let mut last_scope = self.scopes.pop().unwrap();
        last_scope.body.push_str(text);
        self.scopes.push(last_scope);
    }

    /// Append a single char to the current top scope
    fn add_char(&mut self, ch: char) {
        let mut last_scope = self.scopes.pop().unwrap();
        last_scope.body.push(ch);
        self.scopes.push(last_scope);
    }

    /// Add a scope, which may close some existing scopes. The closed scopes are returned in display order.
    /// NOTE: the added infos must live as long as the scope container!
    fn add_scope(&mut self, scope: BBScope<'a>) { 
        //here we assume all taginfos have unique tags because why wouldn't they
        if let Some(topinfo) = self.scopes.last() {
            //oh the thing on top is the same, if we don't want that, close it.
            if topinfo.closes(scope.id) { 
                self.close_last(None);
            }
        }

        self.scopes.push(scope);
    }
    
    /// Close the given scope, which should return the scopes that got closed (including the self).
    /// If no scope could be found, the vector is empty
    fn close_scope(&mut self, id: &'static str) -> usize { 
        let mut scope_count = 0;
        let mut tag_found : bool = false; 

        //Scan backwards, counting. Stop when you find a matching tag. This lets us know the open child scopes
        //that were not closed
        for scope in self.scopes.iter().rev() {
            scope_count += 1;
            if id == scope.id {
                //tag_found = Some(&scope.body);
                tag_found = true;
                break;
            }
        }

        //Return all the scopes from the end to the found closed scope. Oh and also remove them
        if tag_found { 
            for _i in 0..scope_count {
                self.close_last(None); 
            }
            scope_count
        }
        else {
            0 //No scopes closed
        }
    }

    /// Consume the scope system while dumping the rest of the scopes in the right order for display
    fn dump_remaining(mut self) -> String { 
        while self.scopes.len() > 1 {
            self.close_last(None)
        }
        self.scopes.pop().unwrap().emit(None)
    }
}


// ------------------------------
// *     MAIN FUNCTIONALITY    *
// ------------------------------

/// The ways in which you can configure the default link 'target' behavior.
#[derive(Clone, Debug, Default)]
pub enum BBCodeLinkTarget {
    /// Do not generate a 'target' attribute
    None,
    /// Generate a 'target="_blank" attribute
    #[default]
    Blank
}

/// Configuration for tag generation. Generally not necessary, as you can just
/// generate your own tags with more ease and more configuration than this, but
/// this is useful for quick and common modifications to normal tag generation.
#[derive(Clone, Debug)]
pub struct BBCodeTagConfig {
    pub link_target: BBCodeLinkTarget,
    pub img_in_url: bool,
    pub newline_to_br: bool,
    pub accepted_tags: Vec<String>,
    //pub finalize_matchers: Option<Vec<MatchInfo>>
}

impl Default for BBCodeTagConfig {
    /// Produce a default configuration, which includes the basic set of tags and 
    /// (hopefully) expected defaults
    fn default() -> Self {
        Self {
            link_target: BBCodeLinkTarget::default(),
            img_in_url: true,
            newline_to_br: true,
            accepted_tags: BASICTAGS.iter().map(|t| t.to_string()).collect(),
            //finalize_matchers: None
        }
    }
}

impl BBCodeTagConfig {
    /// Produce a default configuration but include the extended tags
    pub fn extended() -> Self {
        let mut config = BBCodeTagConfig::default();
        let mut extags : Vec<String> = EXTENDEDTAGS.iter().map(|t| t.to_string()).collect();
        config.accepted_tags.append(&mut extags);
        config
    }
}

/// The main bbcode system. You create this to parse bbcode! Inexpensive clones,
/// since fields are all reference counted.
#[derive(Clone)] //All the members implement clone
pub struct BBCode {
    /// Supply this!
    pub matchers: Arc<Vec<MatchInfo>>, //These are SOMETIMES processed (based on context)

    #[cfg(feature = "profiling")]
    pub profiler: onestop::OneList<onestop::OneDuration>
}

impl BBCode 
{
    /// Get a default bbcode parser. Should hopefully have reasonable defaults!
    pub fn default() -> Result<Self, Error> {
        Ok(Self::from_config(BBCodeTagConfig::default(), None)?)
    }

    /// Create a BBCode parser from the given list of matchers. If you're building a fully custom set of tags, 
    /// use this endpoint
    pub fn from_matchers(matchers: Vec<MatchInfo>) -> Self {
        Self {
            matchers: Arc::new(matchers),
            #[cfg(feature = "profiling")]
            profiler: onestop::OneList::<onestop::OneDuration>::new()
        }
    }

    /// Create a BBCode parser from a config, using standard tags (plus any extras specified in the config). If you
    /// want a tweaked BBCode parser but based off reasonable defaults, use this. `BBCode::default()` is the same
    /// as calling this with `BBCodeTagConfig::default()`. You can also optionally pass in more tags you wish to support
    /// (this is done because the order of tag matchers is important, and the "standard" configuration requires some internal
    /// matchers to come after yours)
    pub fn from_config(config: BBCodeTagConfig, additional_matchers: Option<Vec<MatchInfo>>) -> Result<Self, Error>
    {
        let mut matches : Vec<MatchInfo> = Vec::new(); 

        //This is an optimization: any large block of characters that has no meaning in bbcode can go straight through.
        matches.push(MatchInfo {
            id: NORMALTEXTID,
            //We use h to catch ourselves on https. this unfortunately breaks up large sections of text into much
            //smaller ones, but it should be ok... I don't know. My parser is stupid lol
            regex: Regex::new(r#"^[^\[\n\rh]+"#)?, 
            match_type : MatchType::Simple(Arc::new(|c| String::from(html_escape::encode_quoted_attribute(&c[0]))))
        });

        //Throw away these characters
        matches.push(MatchInfo { 
            id: CONSUMETEXTID,
            regex: Regex::new(r#"^[\r]+"#)?, 
            match_type: MatchType::Simple(Arc::new(|_c| String::new()))
        });

        let target_attr = match config.link_target {
            BBCodeLinkTarget::Blank => "target=\"_blank\"",
            BBCodeLinkTarget::None => ""
        };
        let target_attr_c1 = target_attr.clone();

        let mut url_only = Self::plaintext_ids();
        if config.img_in_url {
            url_only.push("attr:img")
        }

        let accepted_tags : Vec<&str> = config.accepted_tags.iter().map(|t| t.as_str()).collect();

        macro_rules! addmatch {
            ($name:literal, $value:expr) => {
                addmatch!($name, $value, None, None)
            };
            ($name:literal, $value:expr, $open:expr, $close:expr) => {
                if accepted_tags.contains(&$name) {
                    Self::add_tagmatcher(&mut matches, $name, $value, $open, $close)?
                }
            }
        }

        #[allow(unused_variables)]
        {
            //Basic
            addmatch!("b", ScopeInfo::basic(Arc::new(|o,b,c| format!("<b>{b}</b>"))));
            addmatch!("i", ScopeInfo::basic(Arc::new(|o,b,c| format!("<i>{b}</i>"))));
            addmatch!("sup", ScopeInfo::basic(Arc::new(|o,b,c| format!("<sup>{b}</sup>"))));
            addmatch!("sub", ScopeInfo::basic(Arc::new(|o,b,c| format!("<sub>{b}</sub>"))));
            addmatch!("u", ScopeInfo::basic(Arc::new(|o,b,c| format!("<u>{b}</u>"))));
            addmatch!("s", ScopeInfo::basic(Arc::new(|o,b,c| format!("<s>{b}</s>"))));
            addmatch!("list", ScopeInfo::basic(Arc::new(|o,b,c| format!("<ul>{b}</ul>"))), Some((0,1)), Some((0,1)));
            //There's a [list=1] thing, wonder how to do that. It's nonstandard, our list format is entirely nonstandard
            addmatch!(r"\*", ScopeInfo { 
                only: None, double_closes: true, emit: Arc::new(|o,b,c| format!("<li>{b}</li>"))
            }, Some((1,0)), Some((1,0)));
            addmatch!(r"url", ScopeInfo { 
                only: Some(url_only),
                double_closes: false, 
                emit: Arc::new(move |o,b,c| format!(r#"<a href="{}" {}>{}</a>"#, Self::attr_or_body(&o,b), target_attr, b) )
            });
            addmatch!(r"img", ScopeInfo { 
                only: Some(Self::plaintext_ids()),
                double_closes: false, 
                emit: Arc::new(|o,b,c| format!(r#"<img src="{}">"#, Self::attr_or_body(&o,b)) )
            });

            //Extras
            addmatch!("h1", ScopeInfo::basic(Arc::new(|_o,b,_c| format!("<h1>{}</h1>",b))), Some((0,1)), Some((1,1)));
            addmatch!("h2", ScopeInfo::basic(Arc::new(|_o,b,_c| format!("<h2>{}</h2>",b))), Some((0,1)), Some((1,1)));
            addmatch!("h3", ScopeInfo::basic(Arc::new(|_o,b,_c| format!("<h3>{}</h3>",b))), Some((0,1)), Some((1,1)));
            addmatch!("anchor", ScopeInfo::basic(
                Arc::new(|o,b,_c| format!(r##"<a{} href="#{}">{}</a>"##, Self::attr_or_nothing(&o,"name"), Self::attr_or_body(&o,""), b))));
            addmatch!("quote", ScopeInfo::basic(
                Arc::new(|o,b,_c| format!(r#"<blockquote{}>{}</blockquote>"#, Self::attr_or_nothing(&o,"cite"), b))), Some((0,1)), Some((0,1)));
            addmatch!("spoiler", ScopeInfo::basic(
                Arc::new(|o,b,_c| format!(r#"<details class="spoiler">{}{}</details>"#, Self::tag_or_something(&o,"summary", Some("Spoiler")), b))));
            addmatch!("icode", ScopeInfo {
                only: Some(Self::plaintext_ids()),
                double_closes: false,
                emit: Arc::new(|_o,b,_c| format!(r#"<span class="icode">{b}</span>"#))
            });
            addmatch!("code", ScopeInfo {
                only: Some(Self::plaintext_ids()),
                double_closes: false,
                emit: Arc::new(|o,b,_c| format!(r#"<pre class="code"{}>{}</pre>"#, Self::attr_or_nothing(&o, "data-code"), b) )
            }, Some((0,1)), Some((0,1)));
            addmatch!("youtube", ScopeInfo {
                only: Some(Self::plaintext_ids()),
                double_closes: false,
                emit: Arc::new(|o,b,_c| format!(r#"<a href={} target="_blank" data-youtube>{}</a>"#, Self::attr_or_body(&o, b), b) )
            });
        }

        //WARN: I had to put the newline matcher at the end because of tag newline consumption! This makes configuration
        //and adding new tags a lot more complicated (meaning the user has to pass their matchers in when configuring
        //the parser and they can't be changed)! Unfortunate!
        if let Some(m) = additional_matchers {
            matches.extend(m);
        }

        //The ordering is important! If the user requested <br> output, that needs to come first, so it supercedes
        //the regular newline consumer! But we must include the newline consumer for verbatim sections (like code)
        if config.newline_to_br {
            matches.push(MatchInfo { 
                id: BRNEWLINEID, 
                regex: Regex::new(r#"^\n"#)?, 
                match_type: MatchType::Simple(Arc::new(|_c| String::from("<br>")))
            })
        }
        matches.push(MatchInfo {  //This passes through newlines directly. A catch for when <br> isn't allowed
            id: NEWLINEID, 
            regex: Regex::new(r#"^[\n]+"#)?, 
            match_type: MatchType::Simple(Arc::new(|c| String::from(&c[0])))
        });

        //This new autolinker is taken from 12 since it works better
        let url_chars = r#"[-a-zA-Z0-9_/%&=#+~@$*'!?,.;:]*"#;
        let end_chars = r#"[-a-zA-Z0-9_/%&=#+~@$*']"#;
        let autolink_regex = format!("^https?://{0}{1}([(]{0}[)]({0}{1})?)?", url_chars, end_chars);

        //Don't forget about autolinking! This is a crappy autolinker and it doesn't matter too much!
        matches.push(MatchInfo { 
            id: AUTOLINKID,
            //characters taken from google's page https://developers.google.com/maps/url-encoding
            //NOTE: removed certain characters from autolinking because they SUCK
            regex: Regex::new(&autolink_regex)?,
            match_type: MatchType::Simple(Arc::new(move |c| format!(r#"<a href="{0}" {1}>{0}</a>"#, &c[0], target_attr_c1)))
        });

        Ok(Self::from_matchers(matches))
    }

    /// Convert the current bbcode instance to one which consumes all tags it used to parse. The raw text
    /// SHOULD be left untouched (I think?)
    pub fn to_consumer(&mut self) 
    {
        let new_matchers : Vec<MatchInfo> = 
            self.matchers.iter().map(|m| 
            { 
                match &m.match_type {
                    MatchType::Open(_) | MatchType::Close => {
                        MatchInfo {
                            id: m.id,
                            regex: m.regex.clone(),
                            match_type: MatchType::Simple(Arc::new(|_| String::new()))
                        }
                    },
                    MatchType::Simple(f) => {
                        MatchInfo {
                            id: m.id,
                            regex: m.regex.clone(),
                            match_type: MatchType::Simple(f.clone())
                        }
                    }
                }
            }).collect();
        self.matchers = Arc::new(new_matchers);
    }

    /// Produce the two basic regexes (open and close) for bbcode tags
    pub fn get_tagregex(tag: &'static str, open_consume: Option<(i32,i32)>, close_consume: Option<(i32,i32)>) -> (String, String) 
    {
        let pre_openchomp; let post_openchomp; let pre_closechomp; let post_closechomp;
        match open_consume {
            Some((pre, post)) => {
                pre_openchomp = format!("(?:\r?\n){{0,{}}}", pre);
                post_openchomp = format!("(?:\r?\n){{0,{}}}", post);
            },
            None => {
                pre_openchomp = String::new();
                post_openchomp = String::new();
            }
        }
        match close_consume {
            Some((pre, post)) => {
                pre_closechomp = format!("(?:\r?\n){{0,{}}}", pre);
                post_closechomp = format!("(?:\r?\n){{0,{}}}", post);
            },
            None => {
                pre_closechomp = String::new();
                post_closechomp = String::new();
            }
        }
        let open_tag = format!(r#"^{0}\[{1}((?:[ \t]+{1})?=(?P<attr>[^\]\n]*))?\]{2}"#, pre_openchomp, Self::tag_insensitive(tag), post_openchomp);
        let close_tag = format!(r#"^{}\[/{}\]{}"#, pre_closechomp, Self::tag_insensitive(tag), post_closechomp);
        (open_tag, close_tag)
    }

    /// Add the open and close matches to the given vector for the given tag (you must construct ScopeInfo yourself). 
    /// open_consume and close_consume are the amount of newlines to take before and after the open and close tag
    pub fn add_tagmatcher(matchers: &mut Vec<MatchInfo>, tag: &'static str, info: ScopeInfo, open_consume: Option<(i32,i32)>, close_consume: Option<(i32,i32)>) -> Result<(), Error> { //open_regex: String, close_regex: String) -> Result<(), Error> {
        let (open_tag, close_tag) = Self::get_tagregex(tag, open_consume, close_consume);
        matchers.push(MatchInfo { 
            id: tag, 
            regex: Regex::new(&open_tag)?, 
            match_type: MatchType::Open(Arc::new(info))
        });
        matchers.push(MatchInfo { 
            id: tag, 
            regex: Regex::new(&close_tag)?, 
            match_type: MatchType::Close,
        });
        Ok(())
    }

    /// This is to avoid the unicode requirement, which we don't need to check simple ascii tags
    fn tag_insensitive(tag: &str) -> String {
        let mut result = String::with_capacity(tag.len() * 4);
        let mut skip = 0;
        for c in tag.to_ascii_lowercase().chars() {
            if c == '\\' {
                skip = 2;
            }
            if skip > 0 {
                skip -= 1;
                result.push(c);
                continue;
            }
            result.push_str("[");
            result.push(c);
            result.push(c.to_ascii_uppercase());
            result.push_str("]");
        }
        result
    }
    
    fn attr_or_body(opener: &Option<Captures>, body: &str) -> String {
        if let Some(opener) = opener { 
            if let Some(group) = opener.name("attr") {
                return String::from(html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        return String::from(body);
    }

    fn attr_or_nothing(opener: &Option<Captures>, name: &str) -> String {
        if let Some(opener) = opener {
            if let Some(group) = opener.name("attr") {
                //Note: WE insert the space!
                return format!(" {}=\"{}\"", name, html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        return String::new();
    }

    fn tag_or_something(opener: &Option<Captures>, tag: &str, something: Option<&str>) -> String {
        if let Some(opener) = opener {
            if let Some(group) = opener.name("attr") {
                //Note: WE insert the space!
                return format!("<{0}>{1}</{0}>", tag, html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        if let Some(something) = something {
            return format!("<{0}>{1}</{0}>", tag, html_escape::encode_quoted_attribute(something));
        }
        return String::new();
    }

    pub fn plaintext_ids() -> Vec<&'static str> {
        vec![NORMALTEXTID, CONSUMETEXTID]
    }

    /// Main function! You call this to parse your raw bbcode! It also escapes html stuff so it can
    /// be used raw!  Current version keeps newlines as-is and it's expected you use pre-wrap, later
    /// there may be modes for more standard implementations
    pub fn parse(&self, input: &str) -> String 
    {
        //Because of utf-8, it's better to just use regex directly all the time?
        let mut slice = &input[0..]; //Not necessary to be this explicit ofc

        //Only 'Taginfo' can create scope, so don't worry about "DirectReplace" types
        let mut scoper = BBScoper::new();

        //While there is string left, keep checking against all the regex. Remove some regex
        //if the current scope is a meanie
        while slice.len() > 0
        {
            let mut matched_info : Option<&MatchInfo> = None;

            //figure out which next element matches (if any). This is the terrible optimization part, but
            //these are such small state machines with nothing too crazy that I think it's fine.... maybe.
            //Especially since they all start at the start of the string
            for matchinfo in self.matchers.iter() {
                if !scoper.is_allowed(matchinfo) {
                    continue;
                }
                else if matchinfo.regex.is_match(slice) {
                    matched_info = Some(matchinfo);
                    break;
                }
            }

            //SOMETHING matched, which means we do something special to consume the output
            if let Some(tagdo) = matched_info 
            {
                //There should only be one but whatever
                for captures in tagdo.regex.captures_iter(slice) {
                    slice = &slice[captures[0].len()..];
                    match &tagdo.match_type {
                        MatchType::Simple(closure) => {
                            scoper.add_text(&closure(captures));
                        }
                        MatchType::Open(info) => {
                            //Need to enter a scope. Remember where the beginning of this scope is just in case we need it
                            let new_scope = BBScope {
                                id: tagdo.id,
                                info: info.clone(),
                                open_tag_capture: Some(captures),
                                body: String::new()
                            };
                            scoper.add_scope(new_scope);
                        },
                        MatchType::Close => { 
                            //Attempt to close the given scope. The scoper will return all the actual scopes
                            //that were closed, which we can dump
                            scoper.close_scope(tagdo.id);
                        }
                    }
                }
            }
            else  //Nothing matched, so we just consume the next character. This should be very rare
            {
                //just move forward and emit the char. Note that the slice is in bytes, but the char
                //is a unicode scalar that could be up to 4 bytes, so we need to know how many 'bytes'
                //we just popped off
                if let Some(ch) = slice.chars().next() {
                    scoper.add_char(ch);
                    slice = &slice[ch.len_utf8()..];
                }
                else {
                    println!("In BBCode::parse, there were no more characters but there were leftover bytes!");
                    break;
                }
            }
        }

        scoper.dump_remaining()
    }

    /// This MAY OR MAY NOT profile, depending on your featureset!
    pub fn parse_profiled_opt(&mut self, input: &str, _name: String) -> String 
    {
        #[cfg(feature = "profiling")]
        {
            let mut profile = onestop::OneDuration::new(_name);
            let result = self.parse(input);
            profile.finish();
            self.profiler.add(profile);
            result
        }

        #[cfg(not(feature = "profiling"))]
        return self.parse(input);
    }

}


// ----------------------------
// *         TESTS           
// ----------------------------

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! bbtest_basics {
        ($($name:ident: $value:expr;)*) => {
        $(
            #[test]
            fn $name() {
                let bbcode = BBCode::default().unwrap(); //BBCode::from_matchers(BBCode::basics().unwrap());
                let (input, expected) = $value;
                assert_eq!(bbcode.parse(input), expected);
            }
        )*
        }
    }

    macro_rules! bbtest_extras {
        ($($name:ident: $value:expr;)*) => {
        $(
            #[test]
            fn $name() {
                let mut matchers = Vec::<MatchInfo>::new(); // A list of NEW matchers we'll pass to from_config
                let color_emitter : EmitScope = Arc::new(|open_capture,body,_c| {
                    let color = open_capture.unwrap().name("attr").unwrap().as_str();
                    format!(r#"<span style="color:{}">{}</span>"#, color, body)
                });
                BBCode::add_tagmatcher(&mut matchers, "color", ScopeInfo::basic(color_emitter), None, None).unwrap();
                let bbcode = BBCode::from_config(BBCodeTagConfig::extended(), Some(matchers)).unwrap();
                let (input, expected) = $value;
                assert_eq!(bbcode.parse(input), expected);
            }
        )*
        }
    }

    macro_rules! bbtest_consumer {
        ($($name:ident: $value:expr;)*) => {
        $(
            #[test]
            fn $name() {
                let mut bbcode = BBCode::from_config(BBCodeTagConfig::extended(), None).unwrap();
                bbcode.to_consumer();
                let (input, expected) = $value;
                assert_eq!(bbcode.parse(input), expected);
            }
        )*
        }
    }

    #[test]
    fn build_init() {
        //This shouldn't fail?
        let _bbcode = BBCode::default().unwrap();
    }

    //This isn't really a unit test but whatever
    #[cfg(feature = "bigtest")]
    #[test]
    fn performance_issues() 
    {
        use pretty_assertions::{assert_eq};

        let bbcode = BBCode::from_config(BBCodeTagConfig::extended(), None).unwrap();

        let testdir = "bigtests";
        let entries = std::fs::read_dir(testdir).unwrap();
        let mut checks: Vec<(String,String,String)> = Vec::new();
        for entry in entries 
        {
            let entry = entry.unwrap();
            let path = entry.path();
            let metadata = std::fs::metadata(&path).unwrap();

            //Only look for files
            if metadata.is_file() {
                let base_text = std::fs::read_to_string(&path).unwrap();
                let parse_path = std::path::Path::new(testdir).join("parsed").join(path.file_name().unwrap()); 
                let parse_text = std::fs::read_to_string(&parse_path).unwrap();
                checks.push((base_text, parse_text, String::from(path.file_name().unwrap().to_str().unwrap())));
                println!("Found test file: {:?}", path);
            }
        }
        println!("Total tests: {}", checks.len());
        let start = std::time::Instant::now();
        for (raw, parsed, path) in checks {
            let test_start = start.elapsed();
            let result = bbcode.parse(&raw);
            let test_end = start.elapsed();
            assert_eq!(result, parsed);
            println!(" Test '{}' : {:?}", path, test_end - test_start);
        }
        let elapsed = start.elapsed();
        println!("Parse total: {:?}", elapsed);
    }

    #[cfg(feature = "bigtest")]
    #[test] //Not really a unit test but whatever
    fn benchmark_10000() {
        let bbcode = BBCode::from_config(BBCodeTagConfig::extended(), None).unwrap();
        let parselem = vec![
            ("it's a %CRAZY% <world> 💙=\"yeah\" 👨‍👨‍👧‍👦>>done", 
             "it&#x27;s a %CRAZY% &lt;world&gt; 💙=&quot;yeah&quot; 👨‍👨‍👧‍👦&gt;&gt;done"),
            ("[][[][6][a[ab]c[i]italic[but][][* not] 8[]]][", "[][[][6][a[ab]c<i>italic[but][][* not] 8[]]][</i>"),
            ("[url]this[b]is[/b]a no-no[i][/url]", r#"<a href="this[b]is[/b]a no-no[i]" target="_blank">this[b]is[/b]a no-no[i]</a>"#),
            ("[img=https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png]abc 123[/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">"#),
            ("[spoiler]this[b]is empty[/spoiler]", r#"<details class="spoiler"><summary>Spoiler</summary>this<b>is empty</b></details>"#)
        ];

        let start = std::time::Instant::now();
        for i in 0..10000 {
            if let Some((input, output)) = parselem.get(i % parselem.len()) {
                if bbcode.parse(*input) != *output {
                    panic!("Hang on, bbcode isn't working!");
                }
            }
            else {
                panic!("WHAT? INDEX OUT OF BOUNDS??");
            }
        }
        let elapsed = start.elapsed();
        println!("10000 iterations took: {:?}", elapsed);
    }

    bbtest_basics! {
        no_alter: ("hello", "hello");
        lt_single: ("h<ello", "h&lt;ello");
        gt_single: ("h>ello", "h&gt;ello");
        amp_single: ("h&ello", "h&amp;ello");
        quote_single: ("h'ello", "h&#x27;ello");
        doublequote_single: ("h\"ello", "h&quot;ello");
        return_byebye: ("h\rello", "hello");
        newline_br: ("h\nello", "h<br>ello");
        complex_escape: (
            "it's a %CRAZY% <world> 💙=\"yeah\" 👨‍👨‍👧‍👦>>done", 
            "it&#x27;s a %CRAZY% &lt;world&gt; 💙=&quot;yeah&quot; 👨‍👨‍👧‍👦&gt;&gt;done"
        );
        //"Simple" means there are no complicated tag structures, or only a single tag (most common)
        simple_bold: ("[b]hello[/b]", "<b>hello</b>");
        simple_sup: ("[sup]hello[/sup]", "<sup>hello</sup>");
        simple_sub: ("[sub]hello[/sub]", "<sub>hello</sub>");
        simple_strikethrough: ("[s]hello[/s]", "<s>hello</s>");
        simple_underline: ("[u]hello[/u]", "<u>hello</u>");
        simple_italic: ("[i]hello[/i]", "<i>hello</i>");
        simple_nospaces: ("[b ]hello[/ b]", "[b ]hello[/ b]");
        //The matches are returned lowercase from regex when insensitive
        simple_insensitive: ("[sUp]hello[/SuP]", "<sup>hello</sup>");
        simple_sensitivevalue: ("[sUp]OK but The CAPITALS[/SuP]YEA", "<sup>OK but The CAPITALS</sup>YEA");
        simple_bolditalic: ("[b][i]hello[/i][/b]", "<b><i>hello</i></b>");
        nested_bold: ("[b]hey[b]extra bold[/b] less bold again[/b]", "<b>hey<b>extra bold</b> less bold again</b>");
        simple_url_default: ("[url]https://google.com[/url]", r#"<a href="https://google.com" target="_blank">https://google.com</a>"#);
        simple_url_witharg: ("[url=http://ha4l6o7op9dy.com]furries lol[/url]", r#"<a href="http://ha4l6o7op9dy.com" target="_blank">furries lol</a>"#);
        url_escape: ("[url=http'://ha4l<6o7op9dy>.com]furries lol[/url]", r#"<a href="http&#x27;://ha4l&lt;6o7op9dy&gt;.com" target="_blank">furries lol</a>"#);
        simple_img: ("[img]https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png[/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">"#);
        simple_img_nonstd: ("[img=https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png][/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">"#);
        //NOTE: this one, it's just how I want it to work. IDK how the real bbcode handles this weirdness
        //simple_img_nonstd_inner: ("[img=https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png]abc 123[/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">abc 123"#);
        simple_img_nonstd_inner: ("[img=https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png]abc 123[/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">"#);
        //New in 0.2.0: allow images inside urls (old bbcode matcher I was emulating didn't allow that, but that's stupid)
        url_with_img: ("[url=https://google.com][img]https://some.image.url/junk.png[/img][/url]", r#"<a href="https://google.com" target="_blank"><img src="https://some.image.url/junk.png"></a>"#);
        url_with_img_attr: ("[url=https://google.com][img=https://some.image.url/junk.png][/url]", r#"<a href="https://google.com" target="_blank"><img src="https://some.image.url/junk.png"></a>"#);
        //Note: because url is special and can get the href from either the inside or the attribute, if it has no attribute,
        //it must get it from the inside. So, although the image is a proper tag, it will not be converted in this case
        url_with_img_nourl: ("[url][img=https://some.image.url/junk.png][/url]", r#"<a href="[img=https://some.image.url/junk.png]" target="_blank">[img=https://some.image.url/junk.png]</a>"#);
        url_no_other_tags: ("[url=https://what.non][b][i][u][s][/url]", r#"<a href="https://what.non" target="_blank">[b][i][u][s]</a>"#);
        //Note: this is "undefined" behavior, and it makes sense that an ignored tag (the inner [url]) would not be linked to its immediate
        //closing tag, and so the first [/url] closes the url early. "fixing" this edge case is beyond the scope of this library, as it's 
        //unsupported behavior anyway.
        url_nested: ("[url=https://what.non][url=https://abc123.com][/url][/url]", r#"<a href="https://what.non" target="_blank">[url=https://abc123.com]</a>"#);
        //This also tests auto-closed tags, albeit a simple form
        list_basic:  ("[list][*]item 1[/*][*]item 2[/*][*]list[/*][/list]", "<ul><li>item 1</li><li>item 2</li><li>list</li></ul>");
        unclosed_basic: ("[b] this is bold [i]also italic[/b] oops close all[/i]", "<b> this is bold <i>also italic</i></b> oops close all");
        verbatim_url: ("[url]this[b]is[/b]a no-no[i][/url]", r#"<a href="this[b]is[/b]a no-no[i]" target="_blank">this[b]is[/b]a no-no[i]</a>"#);
        inner_hack: ("[[b][/b]b]love[/[b][/b]b]", "[<b></b>b]love[/<b></b>b]");
        random_brackets: ("[][[][6][a[ab]c[i]italic[but][][* not] 8[]]][", "[][[][6][a[ab]c<i>italic[but][][* not] 8[]]][</i>");
        autolink_basic: ("this is https://google.com ok?", r#"this is <a href="https://google.com" target="_blank">https://google.com</a> ok?"#);

        newline_list1: ("[list]\n[*]item", "<ul><li>item</li></ul>");
        newline_list2: ("[list]\r\n[*]item", "<ul><li>item</li></ul>");
        newline_listmega: ("\n[list]\r\n[*]item\r\n[*]item2 yeah[\r\n\r\n[*]three", "<br><ul><li>item</li><li>item2 yeah[<br></li><li>three</li></ul>");
        //Bold, italic, etc should not remove newlines anywhere
        newline_bold: ("\n[b]\nhellow\n[/b]\n", "<br><b><br>hellow<br></b><br>");
        newline_italic: ("\n[i]\nhellow\n[/i]\n", "<br><i><br>hellow<br></i><br>");
        newline_underline: ("\n[u]\nhellow\n[/u]\n", "<br><u><br>hellow<br></u><br>");
        newline_strikethrough: ("\n[s]\nhellow\n[/s]\n", "<br><s><br>hellow<br></s><br>");
        newline_sup: ("\n[sup]\nhellow\n[/sup]\n", "<br><sup><br>hellow<br></sup><br>");
        newline_sub: ("\n[sub]\nhellow\n[/sub]\n", "<br><sub><br>hellow<br></sub><br>");
        consume_attribute: ("[b=haha ok]but maybe? [/b]{no}", "<b>but maybe? </b>{no}");

        ////Nicole's bbcode edge cases
        e_dangling: ("[b]foo", "<b>foo</b>");
        e_normal: ("[b]foo[/b]", "<b>foo</b>");
        e_nested: ("[b]foo[b]bar[/b][/b]", "<b>foo<b>bar</b></b>");
        e_empty: ("[b]foo[b][/b]bar[/b]", "<b>foo<b></b>bar</b>");
        e_closemulti: ("[b]foo[i]bar[u]baz[/b]quux", "<b>foo<i>bar<u>baz</u></i></b>quux");
        e_faketag: ("[b]foo[i]bar[u]baz[/fake]quux", "<b>foo<i>bar<u>baz[/fake]quux</u></i></b>");
        e_reallyfake: ("[fake][b]foo[i]bar[u]baz[/fake]quux", "[fake]<b>foo<i>bar<u>baz[/fake]quux</u></i></b>");
        e_ignoreclose: ("[b]foo[/b]bar[/b][/b][/b]", "<b>foo</b>bar");
        e_weirdignoreclose: ("[b]foo[/b]bar[/fake][/b][/fake]", "<b>foo</b>bar[/fake][/fake]");
        e_fancytag: ("[[i]b[/i]]", "[<i>b</i>]");
        e_escapemadness: ("&[&]<[<]>[>]", "&amp;[&amp;]&lt;[&lt;]&gt;[&gt;]");
        e_bracket_url: ("[url=#Ports][1][/url]", r##"<a href="#Ports" target="_blank">[1]</a>"##);
    }

    bbtest_extras! {
        e_emptyquote: ("[quote]...[/quote]", "<blockquote>...</blockquote>");
        e_normalquote: ("[quote=foo]...[/quote]", r#"<blockquote cite="foo">...</blockquote>"#);
        simple_spoiler: ("[spoiler=wow]amazing[/spoiler]", r#"<details class="spoiler"><summary>wow</summary>amazing</details>"#);
        simple_emptyspoiler: ("[spoiler]this[b]is empty[/spoiler]", r#"<details class="spoiler"><summary>Spoiler</summary>this<b>is empty</b></details>"#);
        spoiler_simeon: ("[spoiler spoiler=what is this]i hate it[/spoiler]", r#"<details class="spoiler"><summary>what is this</summary>i hate it</details>"#);
        cite_escape: ("[quote=it's<mad>lad]yeah[/quote]",r#"<blockquote cite="it&#x27;s&lt;mad&gt;lad">yeah</blockquote>"#);
        h1_simple: ("[h1] so about that header [/h1]", "<h1> so about that header </h1>");
        h2_simple: (" [h2]Not as important", " <h2>Not as important</h2>");
        h3_simple: ("[h3][h3]wHaAt-Are-u-doin[/h3]", "<h3><h3>wHaAt-Are-u-doin</h3></h3>");
        quote_newlines: ("\n[quote]\n\nthere once was\na boy\n[/quote]\n", "<br><blockquote><br>there once was<br>a boy<br></blockquote>");
        anchor_simple: ("[anchor=Look_Here]The Title[/anchor]", r##"<a name="Look_Here" href="#Look_Here">The Title</a>"##);
        anchor_inside: ("[anchor=name][h1]A title[/h1][/anchor]", r##"<a name="name" href="#name"><h1>A title</h1></a>"##);
        icode_simple: ("[icode=Nothing Yet]Some[b]code[url][/i][/icode]", r#"<span class="icode">Some[b]code[url][/i]</span>"#);
        code_simple: ("\n[code=SB3]\nSome[b]code[url][/i]\n[/code]\n", "<br><pre class=\"code\" data-code=\"SB3\">Some[b]code[url][/i]\n</pre>");
        simple_customtag: ("[color=wow]amazing[/color]", r#"<span style="color:wow">amazing</span>"#);
        simple_customtag_withdefault: ("[color=#FF5500][b][i]ama\nzing[/color]", r#"<span style="color:#FF5500"><b><i>ama<br>zing</i></b></span>"#);
    }

    bbtest_consumer! {
        consume_standard: ("[b]wow[/b] but like [i]uh no scoping [s] rules [/sup] and ugh[/quote]", "wow but like uh no scoping  rules  and ugh");
        //Remember, regex is still the same, so that "code" tag still consumes whitespace
        consume_stillescape: ("<>'\"oof[img=wow][url][code][/url][/code]\n", "&lt;&gt;&#x27;&quot;oof");
    }
/* These tests are limitations of the old parser, I don't want to include them
[quote=foo=bar]...[/quote]
<blockquote>...</blockquote>

[quote=[foo]]...[/quote]
[quote=[foo]]...
*/

}
