use std::sync::Arc;

use regex::{Regex, Captures, Error};

// Carlos Sanchez - 2022-12-05
// - For SBS

//So:
//- early closures close all tags in the previous scope
//- ignore unmatched closing tags
//- close all unclosed tags at the end
//- don't modify whitespace for version 1

static AUTOLINKID: &str = "autolinker";
static CONSUMETEXTID : &str = "consumetext";
static NORMALTEXTID: &str = "normaltext";

pub type EmitScope = Box<dyn Fn(Option<Captures>, &str, Option<Captures>)->String>; //, //The emmitter for this entire scope

/// A structure to build a bbbcode tag. Nearly all bbcode tags should work through this configuration,
/// but you may find some are too complex (for now)
//#[derive(Debug, Clone)]
//pub struct TagInfo {
//    ///The tag identity, such as "b", "youtube", etc
//    pub tag : &'static str,
//    //The tag to put out as html
//    pub outtag: &'static str,
//    pub tag_type: TagType,
//    pub rawextra: Option<&'static str>, //Just dump this directly into the tag at the end. No checks performed
//    pub valparse: TagValueParse,
//    pub blankconsume : BlankConsume
//}
//
//impl TagInfo {
//    /// Constructors for basic tags. Anything else, you're better off just constructing it normally
//    pub fn simple(tag: &'static str) -> TagInfo {
//        TagInfo { tag, outtag: tag, tag_type: TagType::Simple, rawextra: None, valparse: TagValueParse::Normal, blankconsume: BlankConsume::None }
//    }
//    /*pub fn normal(tag: &'static str, outtag: &'static str, tag_type: TagType, rawextra: Option<&'static str>) -> TagInfo {
//        TagInfo { tag, outtag, tag_type, rawextra, valparse
//    }*/
//    fn start() -> TagInfo {
//        TagInfo { tag: "", outtag: "", tag_type: TagType::Start, rawextra: None, valparse: TagValueParse::Normal, blankconsume: BlankConsume::None }
//    }
//
//    /// Does this tag allow other tags inside, or none? Some tag types automatically require no other 
//    /// tags inside, while others can manually set themselves to verbatim
//    pub fn is_verbatim(&self) -> bool {
//        if let TagValueParse::ForceVerbatim = self.valparse {
//            true
//        }
//        else {
//            match self.tag_type {
//                TagType::Start => false,
//                TagType::Simple => false,
//                TagType::DefinedArg(_) => false,
//                TagType::DefinedTag(_, _) => false,
//                TagType::SelfClosing(_) => true,
//                TagType::DefaultArg(_) => true
//            }
//        }
//    }
//}
//
///// How are values parsed? Basically varying levels of rigidity in having tags
///// inside other tags
//#[derive(Debug, Clone)]
//pub enum TagValueParse {
//    Normal,
//    ForceVerbatim,
//    DoubleCloses
//}
//
///// This is the 'silly' part of the parser. Rather than making some actually generic system, I identified some
///// standard ways tags are used and just made code around those ways. Probably bad but oh well.
//#[derive(Debug, Clone)]
//pub enum TagType {
//    /// Don't make tags with this type, it's for the system! Should ONLY have one of these! It's like S in a grammar!
//    Start,          
//    /// Stuff like [b][/b], no args, normal translation (can change tag name still)
//    Simple,         
//    /// CAN have argument defined (but optional), attribute name is given in enum
//    DefinedArg(&'static str),   
//    /// Some arguments turn into tags! Crazy... (like spoiler creating a "summary" tag)
//    DefinedTag(&'static str, Option<&'static str>),   
//    /// No closing tag, value turns into an arg with the name given in the enum
//    SelfClosing(&'static str),  
//    /// The tag enclosed value provides a default for the given attribute, or not if defined. Used for [url] etc
//    DefaultArg(&'static str),   
//}
//



pub struct ScopeInfo {
    pub only: Option<Vec<&'static str>>,
    pub double_closes: bool,
    pub emit: EmitScope, 
}

impl Default for ScopeInfo {
    fn default() -> Self {
        Self {
            only: None,
            double_closes: false,
            emit: Box::new(|_o, b, _c| String::from(b))
        }
    }
}

impl ScopeInfo {
    fn basic(emitter: EmitScope) -> Self {
        let mut result = Self::default();
        result.emit = emitter;
        result
    }
}

/// While "TagType" determines how the tag functions at a lower level (such as how it handles arguments), 
/// this determines how the whole block functions on a greater level. They define how scopes and whole blocks
/// of text move into the output. This still operates on the idea of "tags" though
pub enum MatchType { 
    /// A match type that requires no scoping rules: just a simple regex replacement (or whatever replacement you want)
    Simple(Box<dyn Fn(Captures)->String>), //Inefficient to use String but we have to in order to support replacements etc
    /// The match should expect an open tag, which increases scope and performs open scope rules
    Open(Arc<ScopeInfo>), 
    /// The match should expect a closing tag, which decreases scope and performs close scope rules
    Close
}

/// Definition for a block level matcher. Analogous to "TypeInfo" but for the greater scope. Should always be
/// readonly, it is just a definition. Not necessary a tag element, could define eating garbage, escape chars, etc.
pub struct MatchInfo {
    pub id: &'static str,
    pub regex : Regex,
    pub match_type: MatchType,
}

//For the following section, it is assumed the entire scoper and all elements within will have the same lifetime
//as the calling function, which also houses the input string.

/// A scope for bbcode tags. Scopes increase and decrease as tags are opened and closed. Scopes are placed on a stack
/// to aid with auto-closing tags
struct BBScope<'a> {
    id: &'static str,
    info: Arc<ScopeInfo>,
    open_tag_capture: Option<Captures<'a>>,
    body: String, //where to dump current result (when this scope is one top)
}

impl BBScope<'_> {
    fn is_allowed(&self, id: &str) -> bool {
        if let Some(only) = &self.info.only {
            id == self.id || only.contains(&id)
        }
        else {
            true
        }
    }
    fn double_closes(&self, id: &str) -> bool {
        self.id == id && self.info.double_closes
    }
    fn emit(self, close_captures: Option<Captures>) -> String {
        let emitter = &self.info.emit;
        emitter(self.open_tag_capture, &self.body, close_captures)
    }
}

/// A container with functions to help manage scopes. It doesn't understand what bbcode is or how the tags should
/// be formatted, it just handles pushing and popping scopes on the stack
struct BBScoper<'a> {
    //start_info: ScopeInfo,
    scopes : Vec<BBScope<'a>>
}

/// Everything inside BBScoper expects to live as long as the object itself. So everything is 'a
impl<'a> BBScoper<'a> 
{
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

    fn is_allowed(&self, id: &str) -> bool {
        self.scopes.last().unwrap().is_allowed(id)
    }

    fn close_last(&mut self, close_tag: Option<Captures>) {
        if let Some(scope) = self.scopes.pop() {
            let body = scope.emit(close_tag); //this consumes the scope, which is fine
            self.add_text(&body);
        }
        else {
            panic!("BBScoper::close_last HOW DID THIS HAPPEN? There were scopes from .last but pop returned none!");
        }
    }

    fn add_text(&mut self, text: &str) {
        let mut last_scope = self.scopes.pop().unwrap();
        last_scope.body.push_str(text);
        self.scopes.push(last_scope);
    }

    /// Add a scope, which may close some existing scopes. The closed scopes are returned in display order.
    /// NOTE: the added infos must live as long as the scope container!
    fn add_scope(&mut self, scope: BBScope<'a>) { // -> &BBScope {// Vec<BBScope<'a>>) {
        //here we assume all taginfos have unique tags because why wouldn't they
        //let mut result = Vec::new();
        if let Some(topinfo) = self.scopes.last() {
            //oh the thing on top is the same, if we don't want that, close it.
            if topinfo.double_closes(scope.id) { //topinfo.info.tag == scope.info.tag && matches!(scope.info.valparse, TagValueParse::DoubleCloses){
                self.close_last(None);
            }
        }

        self.scopes.push(scope);
        //(self.scopes.last().unwrap(), result) //Kind of a silly return type, might change it later
    }
    
    /// Close the given scope, which should return the scopes that got closed (including the self).
    /// If no scope could be found, the vector is empty
    fn close_scope(&mut self, id: &'static str) -> usize { //info: &'a TagInfo) { //-> Vec<BBScope<'a>> {
        let mut scope_count = 0;
        let mut tag_found : bool = false; //Option<&String> = None; //false;

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
        if tag_found { //let //Some(body) = tag_found {
            //let mut result = Vec::with_capacity(scope_count + 1);
            for _i in 0..scope_count {
                self.close_last(None); //close_tag)
                //if let Some(scope) = self.scopes.pop() {
                //    result.push(scope);
                //}
                //else {
                //    println!("BBScope::close_scope LOGIC ERROR: SCANNED PAST END OF SCOPELIST");
                //}
            }
            //result
            scope_count
        }
        else {
            0 //No scopes closed
            //Vec::new()
        }
    }

    /// Consume the scope system while dumping the rest of the scopes in the right order for display
    fn dump_remaining(mut self) -> String { //Vec<BBScope<'a>> {
        while self.scopes.len() > 1 {
            self.close_last(None)
        }
        self.scopes.pop().unwrap().emit(None)
    }
}


// ------------------------------
// *     MAIN FUNCTIONALITY    *
// ------------------------------

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
        Ok(Self::from_matchers(Self::basics()?))
    }

    pub fn from_matchers(matchers: Vec<MatchInfo>) -> Self {
        Self {
            matchers: Arc::new(matchers),
            #[cfg(feature = "profiling")]
            profiler: onestop::OneList::<onestop::OneDuration>::new()
        }
    }

    /// Produce the two basic regexes (open and close) for bbcode tags
    pub fn get_tagregex(tag: &'static str, open_consume: Option<(i32,i32)>, close_consume: Option<(i32,i32)>) -> (String, String) 
    {
        let pre_openchomp; let post_openchomp; let pre_closechomp; let post_closechomp;
        match open_consume {
            Some((pre, post)) => {
                pre_openchomp = format!("(\r?\n){{0,{}}}", pre);
                post_openchomp = format!("(\r?\n){{0,{}}}", post);
            },
            None => {
                pre_openchomp = String::new();
                post_openchomp = String::new();
            }
        }
        match close_consume {
            Some((pre, post)) => {
                pre_closechomp = format!("(\r?\n){{0,{}}}", pre);
                post_closechomp = format!("(\r?\n){{0,{}}}", post);
            },
            None => {
                pre_closechomp = String::new();
                post_closechomp = String::new();
            }
        }
        let open_tag = format!(r#"^{}\[{}(=([^\]\n]*))?\]{}"#, pre_openchomp, Self::tag_insensitive(tag), post_openchomp);
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
    
    fn attr_or_body(opener: Option<Captures>, body: &str) -> String {
        if let Some(opener) = opener { //}.len()
            if let Some(group) = opener.get(2) {
                return String::from(html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        return String::from(body);
    }

    fn attr_or_nothing(opener: Option<Captures>, name: &str) -> String {
        if let Some(opener) = opener {
            if let Some(group) = opener.get(2) {
                //Note: WE insert the space!
                return format!(" {}=\"{}\"", name, html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        return String::new();
    }

    fn tag_or_something(opener: Option<Captures>, tag: &str, something: Option<&str>) -> String {
        if let Some(opener) = opener {
            if let Some(group) = opener.get(2) {
                //Note: WE insert the space!
                return format!("<{0}>{1}</{0}>", tag, html_escape::encode_quoted_attribute(group.as_str()));
            }
        }
        if let Some(something) = something {
            return format!("<{0}>{1}</{0}>", tag, html_escape::encode_quoted_attribute(something));
        }
        return String::new();
    }

    /// Get a vector of ALL basic matchers! This is the function you want to call to get a vector for the bbcode
    /// generator!
    pub fn basics() -> Result<Vec<MatchInfo>, regex::Error> {
        //First, get the default direct replacements
        let mut matches : Vec<MatchInfo> = Vec::new(); 

        //This is an optimization: any large block of characters that has no meaning in bbcode can go straight through.
        matches.push(MatchInfo {
            id: NORMALTEXTID,
            //We use h to catch ourselves on https. this unfortunately breaks up large sections of text into much
            //smaller ones, but it should be ok... I don't know. My parser is stupid lol
            regex: Regex::new(r#"^[^\[\n\rh]+"#)?, 
            match_type : MatchType::Simple(Box::new(|c| String::from(html_escape::encode_quoted_attribute(&c[0]))))
        });

        matches.push(MatchInfo { 
            id: CONSUMETEXTID,
            regex: Regex::new(r#"^[\r]+"#)?, 
            match_type: MatchType::Simple(Box::new(|_c| String::new()))
        });


        #[allow(unused_variables)]
        {
            Self::add_tagmatcher(&mut matches, "b", ScopeInfo::basic(Box::new(|o,b,c| format!("<b>{}</b>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "i", ScopeInfo::basic(Box::new(|o,b,c| format!("<i>{}</i>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "sup", ScopeInfo::basic(Box::new(|o,b,c| format!("<sup>{}</sup>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "sub", ScopeInfo::basic(Box::new(|o,b,c| format!("<sub>{}</sub>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "u", ScopeInfo::basic(Box::new(|o,b,c| format!("<u>{}</u>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "s", ScopeInfo::basic(Box::new(|o,b,c| format!("<s>{}</s>",b))), None, None)?;
            Self::add_tagmatcher(&mut matches, "list", ScopeInfo::basic(Box::new(|o,b,c| format!("<ul>{}</ul>",b))), Some((0,1)), Some((0,1)))?;
            Self::add_tagmatcher(&mut matches, r"\*", ScopeInfo { 
                only: None, double_closes: true, emit: Box::new(|o,b,c| format!("<li>{}</li>",b))
            }, Some((1,0)), Some((1,0)))?;
            Self::add_tagmatcher(&mut matches, r"url", ScopeInfo { 
                only: Some(vec![NORMALTEXTID, CONSUMETEXTID]), 
                double_closes: false, 
                emit: Box::new(|o,b,c| format!(r#"<a href="{}" target="_blank">{}</a>"#, Self::attr_or_body(o,b), b) )
            }, None, None)?;
            Self::add_tagmatcher(&mut matches, r"img", ScopeInfo { 
                only: Some(vec![NORMALTEXTID, CONSUMETEXTID]),
                double_closes: false, 
                emit: Box::new(|o,b,c| format!(r#"<img src="{}">"#, Self::attr_or_body(o,b)) )
            }, None, None)?;
        }

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
            match_type: MatchType::Simple(Box::new(|c| format!(r#"<a href="{0}" target="_blank">{0}</a>"#, &c[0])))
        });

        //There's a [list=1] thing, wonder how to do that. It's nonstandard, our list format is entirely nonstandard
        Ok(matches)
    }

    /// Some fancy extra bbcode. Does not include basics! These are nonstandard, you don't have to use them!
    pub fn extras() -> Result<Vec<MatchInfo>, Error> {
        let mut matches : Vec<MatchInfo> = Vec::new(); 
        Self::add_tagmatcher(&mut matches, "h1", ScopeInfo::basic(Box::new(|_o,b,_c| format!("<h1>{}</h1>",b))), None, None)?;
        Self::add_tagmatcher(&mut matches, "h2", ScopeInfo::basic(Box::new(|_o,b,_c| format!("<h2>{}</h2>",b))), None, None)?;
        Self::add_tagmatcher(&mut matches, "h3", ScopeInfo::basic(Box::new(|_o,b,_c| format!("<h3>{}</h3>",b))), None, None)?;
        Self::add_tagmatcher(&mut matches, r"quote", ScopeInfo { 
            only: None,
            double_closes: false, 
            emit: Box::new(|o,b,_c| format!(r#"<blockquote{}>{}</blockquote>"#, Self::attr_or_nothing(o,"cite"), b) )
        }, None, None)?;
        Self::add_tagmatcher(&mut matches, r"spoiler", ScopeInfo { 
            only: None,
            double_closes: false, 
            emit: Box::new(|o,b,_c| format!(r#"<details class="spoiler">{}{}</details>"#, Self::tag_or_something(o,"summary", Some("Spoiler")), b) )
        }, None, None)?;
        Ok(matches)
        //BBCode::tags_to_matches(vec![
        //    TagInfo { tag: "quote", outtag: "blockquote", tag_type : TagType::DefinedArg("cite"), rawextra : None, valparse: TagValueParse::Normal, blankconsume: BlankConsume::End(1) },
        //    TagInfo { tag: "anchor", outtag: "a", tag_type : TagType::DefinedArg("name"), rawextra : None, valparse: TagValueParse::ForceVerbatim, blankconsume: BlankConsume::None },
        //    TagInfo { tag: "icode", outtag: "span", tag_type : TagType::Simple, rawextra : Some(r#"class="icode""#), valparse: TagValueParse::ForceVerbatim, blankconsume: BlankConsume::None },
        //    TagInfo { tag: "code", outtag: "pre", tag_type : TagType::DefinedArg("data-code"), rawextra : Some(r#"class="code""#), valparse: TagValueParse::ForceVerbatim, blankconsume: BlankConsume::End(1) },
        //    TagInfo { tag: "youtube", outtag: "a", tag_type : TagType::DefaultArg("href"), rawextra : Some(r#"class="youtube" data-youtube"#), valparse: TagValueParse::ForceVerbatim, blankconsume: BlankConsume::End(1) },
        //    TagInfo { tag: "spoiler", outtag: "details", tag_type : TagType::DefinedTag("summary", Some("Spoiler")), rawextra : Some(r#"class="spoiler""#), valparse: TagValueParse::Normal, blankconsume: BlankConsume::None },
        //    TagInfo::simple("h1"),
        //    TagInfo::simple("h2"),
        //    TagInfo::simple("h3"),
        //])
    }

    ///// Push an argument="value" part onto the result. Will omit the last " if argval is None
    //fn push_tagarg(mut result: String, argname: &str, argval: Option<&str>) -> String {
    //    result.push_str(" ");
    //    result.push_str(argname);
    //    result.push_str("=\"");
    //    //Now we need an html escaper
    //    if let Some(argval) = argval {
    //        //NOTE: our matcher grabs the = (for now), that's why the 1
    //        result.push_str(&html_escape::encode_quoted_attribute(argval));
    //        result.push_str("\"");
    //    }
    //    result
    //}

    //fn push_newtag(mut result: String, tagname: &str, argval: &str) -> String {
    //    //Close the old tag, open a new one
    //    result.push_str("><");
    //    result.push_str(tagname);
    //    result.push_str(">");
    //    //NOTE: our matcher grabs the = (for now), that's why the 1
    //    result.push_str(&html_escape::encode_quoted_attribute(argval));
    //    //And close the whole thing off
    //    result.push_str("</");
    //    result.push_str(tagname);
    //    result.push_str(">");
    //    result
    //}

    ///// Write the "open" tag to the given result for the given new scope. 
    //fn push_open_tag(mut result: String, scope: &BBScope, captures: &Captures) -> String {
    //    result.push_str("<");
    //    result.push_str(scope.info.outtag);
    //    //Put the raw stuff first (maybe class, other)
    //    if let Some(rawextra) = scope.info.rawextra {
    //        result.push_str(" ");
    //        result.push_str(rawextra);
    //    }
    //    //Now output different stuff depending on the type
    //    match scope.info.tag_type {
    //        TagType::Start => {}, //Do nothing
    //        TagType::Simple => { result.push_str(">"); }, //Just close it, all done!
    //        TagType::DefinedArg(argname) => {
    //            if let Some(capture) = captures.get(1) { //Push the argument first
    //                result = Self::push_tagarg(result, argname, Some(&capture.as_str()[1..]));
    //            }
    //            result.push_str(">"); //THEN close it!
    //        },
    //        TagType::DefinedTag(tagname, default) => { //These make the argument a new tag 
    //            if let Some(capture) = captures.get(1) { //Push the argument first
    //                result = Self::push_newtag(result, tagname, &capture.as_str()[1..]); //+1 here because it's not some?
    //            }
    //            else if let Some(default) = default {
    //                result = Self::push_newtag(result, tagname, default);
    //            }
    //            else {
    //                result.push_str(">"); //If we didn't push a new arg, gotta close the existing tag
    //            }
    //        },
    //        //For the opening tag, 'DefaultArg' and 'SelfClosing' act the same. They could either have the value
    //        //in the arg, or in the body. Difference is on completion, where self closing just closes (or quits),
    //        //but DefaultArg may have to copy the value into the body, since we only scanned the arg
    //        TagType::SelfClosing(argname) | TagType::DefaultArg(argname) => {
    //            if let Some(capture) = captures.get(1) { //If an argument exists, push it
    //                result = Self::push_tagarg(result, argname, Some(&capture.as_str()[1..]));
    //                result.push_str(">"); //THEN close it!
    //            }
    //            else {  
    //                //But if it doesn't, output like it's a SelfClosing, meaning the inner value
    //                //in bbcode becomes the 'default arg'. This requires a special handler in the
    //                //closing tag
    //                result = Self::push_tagarg(result, argname, None);
    //            }
    //        },
    //    }

    //    result
    //}

    //fn push_just_close_tag(mut result: String, info: &TagInfo) -> String {
    //    result.push_str("</");
    //    result.push_str(info.outtag);
    //    result.push_str(">");
    //    result
    //}

    ///// Emit the closing tag for the given scope. This also needs the full input string and the position
    ///// of the end of this scope, because certain complicated closing tags need it.
    //fn push_close_tag(mut result: String, scope: &BBScope, input: &str, end: usize) -> String {
    //    match scope.info.tag_type {
    //        TagType::SelfClosing(_) => {
    //            if !scope.has_arg { 
    //                //If this was the standard style of selfclosing, need output the end of the tag.
    //                //If it was in the arguments (nonstandard), we already closed it
    //                result.push_str(r#"">"#);
    //            }
    //        },
    //        TagType::DefaultArg(_) => {
    //            //This one is complicated. If there were arguments, we simply output the closing 
    //            //tag, same as a normal tag. But if there was NOT an argument, we're still in the original
    //            //tag, AND we still have to output the value we captured in this scope
    //            if scope.has_arg {
    //                result = Self::push_just_close_tag(result, scope.info);
    //            }
    //            else {
    //                result.push_str(r#"">"#);
    //                result.push_str(&input[scope.inner_start..end]);
    //                result = Self::push_just_close_tag(result, scope.info);
    //            }
    //        }
    //        TagType::Start => {
    //            //Do nothing
    //        },
    //        _ => {
    //            result = Self::push_just_close_tag(result, scope.info);
    //        }
    //    }

    //    result
    //}

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
                if !scoper.is_allowed(matchinfo.id) {
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
                    //do this pre-emptively so we're AT the start of the inside of the tag
                    //let scope_end : usize = slice.as_ptr() as usize - input.as_ptr() as usize;
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
                        MatchType::Close => { //(info) => {
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
                    //TODO: OMG FIX THIS
                    scoper.add_text(&format!("{}", ch));
                    //current_scope.body.push(ch);
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
                let bbcode = BBCode::from_matchers(BBCode::basics().unwrap());
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
                let mut matchers = BBCode::basics().unwrap();
                let mut extras = BBCode::extras().unwrap();
                matchers.append(&mut extras);
                let bbcode = BBCode::from_matchers(matchers);
                let (input, expected) = $value;
                assert_eq!(bbcode.parse(input), expected);
            }
        )*
        }
    }

    #[test]
    fn build_init() {
        //This shouldn't fail?
        let _bbcode = BBCode::from_matchers(BBCode::basics().unwrap());
    }

    //This isn't really a unit test but whatever
    #[cfg(feature = "bigtest")]
    #[test]
    fn performance_issues() 
    {
        use pretty_assertions::{assert_eq};

        let mut matchers = BBCode::basics().unwrap();
        let mut extras = BBCode::extras().unwrap();
        matchers.append(&mut extras);
        let bbcode = BBCode::from_matchers(matchers);

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
        let mut matchers = BBCode::basics().unwrap();
        let mut extras = BBCode::extras().unwrap();
        matchers.append(&mut extras);
        let bbcode = BBCode::from_matchers(matchers);
        let parselem = vec![
            ("it's a %CRAZY% <world> 💙=\"yeah\" 👨‍👨‍👧‍👦>>done", 
             "it&#39;s a %CRAZY% &lt;world&gt; 💙=&quot;yeah&quot; 👨‍👨‍👧‍👦&gt;&gt;done"),
            ("[][[][6][a[ab]c[i]italic[but][][* not] 8[]]][", "[][[][6][a[ab]c<i>italic[but][][* not] 8[]]][</i>"),
            ("[url]this[b]is[/b]a no-no[i][/url]", r#"<a target="_blank" href="this[b]is[/b]a no-no[i]">this[b]is[/b]a no-no[i]</a>"#),
            ("[img=https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png]abc 123[/img]", r#"<img src="https://old.smiflebosicswoace.com/user_uploads/avatars/t1647374379.png">abc 123"#),
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
        //Because inserting tags without knowing the scope is bad (in our system for now), don't generate
        //<br>, just figure the whitespace is pre-wrap or something
        newline_br: ("h\nello", "h\nello");
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
        //This also tests auto-closed tags, albeit a simple form
        list_basic:  ("[list][*]item 1[/*][*]item 2[/*][*]list[/*][/list]", "<ul><li>item 1</li><li>item 2</li><li>list</li></ul>");
        unclosed_basic: ("[b] this is bold [i]also italic[/b] oops close all[/i]", "<b> this is bold <i>also italic</i></b> oops close all");
        verbatim_url: ("[url]this[b]is[/b]a no-no[i][/url]", r#"<a href="this[b]is[/b]a no-no[i]" target="_blank">this[b]is[/b]a no-no[i]</a>"#);
        inner_hack: ("[[b][/b]b]love[/[b][/b]b]", "[<b></b>b]love[/<b></b>b]");
        random_brackets: ("[][[][6][a[ab]c[i]italic[but][][* not] 8[]]][", "[][[][6][a[ab]c<i>italic[but][][* not] 8[]]][</i>");
        autolink_basic: ("this is https://google.com ok?", r#"this is <a href="https://google.com" target="_blank">https://google.com</a> ok?"#);

        newline_list1: ("[list]\n[*]item", "<ul><li>item</li></ul>");
        newline_list2: ("[list]\r\n[*]item", "<ul><li>item</li></ul>");
        newline_listmega: ("\n[list]\r\n[*]item\r\n[*]item2 yeah[\r\n\r\n[*]three", "\n<ul><li>item</li><li>item2 yeah[\n</li><li>three</li></ul>");
        //Bold, italic, etc should not remove newlines anywhere
        newline_bold: ("\n[b]\nhellow\n[/b]\n", "\n<b>\nhellow\n</b>\n");
        newline_italic: ("\n[i]\nhellow\n[/i]\n", "\n<i>\nhellow\n</i>\n");
        newline_underline: ("\n[u]\nhellow\n[/u]\n", "\n<u>\nhellow\n</u>\n");
        newline_strikethrough: ("\n[s]\nhellow\n[/s]\n", "\n<s>\nhellow\n</s>\n");
        newline_sup: ("\n[sup]\nhellow\n[/sup]\n", "\n<sup>\nhellow\n</sup>\n");
        newline_sub: ("\n[sub]\nhellow\n[/sub]\n", "\n<sub>\nhellow\n</sub>\n");

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
    }

    bbtest_extras! {
        e_emptyquote: ("[quote]...[/quote]", "<blockquote>...</blockquote>");
        e_normalquote: ("[quote=foo]...[/quote]", r#"<blockquote cite="foo">...</blockquote>"#);
        simple_spoiler: ("[spoiler=wow]amazing[/spoiler]", r#"<details class="spoiler"><summary>wow</summary>amazing</details>"#);
        simple_emptyspoiler: ("[spoiler]this[b]is empty[/spoiler]", r#"<details class="spoiler"><summary>Spoiler</summary>this<b>is empty</b></details>"#);
        cite_escape: ("[quote=it's<mad>lad]yeah[/quote]",r#"<blockquote cite="it&#x27;s&lt;mad&gt;lad">yeah</blockquote>"#);
    }

/* These tests are limitations of the old parser, I don't want to include them
[quote=foo=bar]...[/quote]
<blockquote>...</blockquote>

[quote=[foo]]...[/quote]
[quote=[foo]]...
*/

}