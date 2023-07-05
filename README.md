# More BBCode parsers?

Yeah! I needed something highly extensible, flexible, and specifically WITH scoping 
rules so it always produces correct HTML. For instance, there's stuff like:

```
[b]This is bold! [i]AND ITALIC?[/b] oops where's [/i]?
```

Where you have unmatching closing tags. While a simple regex replacement may handle this
any number of ways, this library will produce:

```html
<b>This is bold! <i>AND ITALIC?</i></b> oops where&#x27;s ?
```

Another example:

```
And so [s] I'm just like [s] opening tons of [sup] tags?
Maybe I'll close this [/s] one
```

```html
And so <s> I&#x27;m just like <s> opening tons of <sup> tags?<br>Maybe I&#x27;ll close this </sup></s> one</s>
```

All unclosed tags are automatically closed in the correct order, including any that were
left at the end of the text. Unmatched closing tags are removed. Of course, this may not
be what you want, but I've found that most older or established bbcode parsers work this
way. With this library, you can feel (generally) safe knowing it will produce proper HTML.

With scoping rules, you also get access to tags which can reject other tags inside of them,
by specifying the `only` vector (more later). For instance, in the extended tagset, I have 
[code] which rejects all types of matches except normal text and "garbage" (characters we
throw away, like \r).

## Quickstart 

```rust
let bbcode = BBCode::default().unwrap(); // Or catch error
let html = bbcode.parse("[url]https://github.com[/url]")
// Make sure to reuse the BBCode you created! Creating is expensive, but cloning is cheap!
let bbcode2 = bbcode.clone();
```

Or, if you want the extended set (see next section for list):

```rust
// Instead of requesting the default BBCode parser, you can pass in a configuration
// for the most common alterations, along with a vector of extra tag parsers (see later)
let mut bbcode = BBCode::from_config(BBCodeTagConfig::extended(), None).unwrap();
let html = bbcode.parse("[code]wow\nthis is code![/code]");
```

Or, if you want to add your own tags:

```rust
let mut config = BBCodeTagConfig::default(); // Default does not include extended tags fyi
let mut matchers = Vec<MatchInfo>::new(); // A list of NEW matchers we'll pass to from_config

// You define how your tag gets turned into HTML using a closure; you are provided the open tag 
// regex capture, the pre-parsed pre-escaped body, and the closing tag regex capture (if the user provided it).
// "EmitScope" is just a fancy alias so you don't have to fuss with the complicated types
let color_emitter : EmitScope = Arc::new(|open_capture,body,_c| {
  //NOTE: in production code, don't `unwrap` the named capture group, it might not exist!
  let color = open_capture.unwrap().name("attr").unwrap().as_str();
  format!(r#"<span style="color:{}">{}</span>"#, color, body)
});

BBCode::add_tagmatcher(&mut matchers, "color", ScopeInfo::basic(color_emitter), None, None)?;
//Repeat the emitter / add_tagmatcher above for each tag you want to add

let bbcode = BBCode::from_config(config, Some(matchers));
```

The `BBCode::add_tagmatcher` method constructs a bbcode tag parser for you using less code than
normally required, but you can technically construct your own matcher manually which can match 
almost anything. For now, if you're just trying to add basic bbcode tags, you'll see in the above:
- First parameter is the list to append the matcher to (it adds multiple items).
- Second is the name of the bbcode tag, all lowercase (so this would match [color])
- Third is a special "ScopeInfo" struct, but we're calling the "basic" constructor and simply
  passing a boxed closure rather than configuring the entire ScopeInfo.
- That boxed closure is a so-called `EmitScope`, which is that closure you wrote that gives you 
  the regex capture for the open tag, the pre-html-escaped pre-parsed body, and the closing tag 
  regex capture, which you can use to output (emit) the constructed html. Note that, although the 
  opening tag is nearly always given, the closing tag is OFTEN not given, especially if the user 
  did not close their tags. Do not rely on the last parameter (`_c` in the example) existing
- Note that the opening tag capture has a named group called `attr`, which is the value of the
  attribute given in the bbcode tag. For instance, if you had `[url=http://whatever]abc[/url]`, 
  the match `attr` would house the string `http://whatever` (NOT pre-escaped, be careful!)
- The last two parameters are optional newline consumption before and after the opening and 
  closing tag. For instance, if you wanted to consume the first newline __before__ the opening tag, and
  the first newline __after__ the closing tag, those two might look like `Some((1,0)), Some((0,1))`
  (this may change in the future)
  
### Rocket Web example
There are many web frameworks to choose from for rust, so having an example for each would be a 
bit difficult. Someone suggested Rocket, so here's an example in 0.5.0_rc2:

```rust
#[macro_use] extern crate rocket;
use rocket::response::content;
use bbscope::BBCode;

#[launch]
fn rocket() -> _ {
    let bbcode = BBCode::default().unwrap();
    rocket::build()
      .mount("/", routes![ index ])
      .manage(bbcode) //Add as state, you want to reuse the bbcode object!!
}

#[get("/")]
fn index(bbcode: &State<BBCode>) -> content::RawHtml<String> {
  content::RawHtml(bbcode.parse("Hey, it's [b]bbcode[/b]! [i]Oops, [u]forgot to close[/i] a tag"))
}
```


## Default supported tags:

BBCode is so varied, there's so many crazy tags and systems and nobody was ever able to agree
on any, or at least if they did, it was too late. These are the tags supported in the
'basic' set:

- [b]bold[/b]
- [i]italic[/i]
- [s]strikethrough[/s]
- [u]underline[/u]
- [sup]superscript[/sup]
- [sub]subscript[/sub]
- [url=link*]url[/url] (=link attribute optional)
- [img=link*]link[/img] (only one needed: attribute or inner)
- [list][\*]item[\*]item2[/list]

Some of those may be nonstandard, and you may be missing some you find standard! If so,
there's also an optional extended list:

- [quote=cite*]a blockquote[/quote] (=cite attribute optional)
- [code]verbatim pre[/code]
- [icode]verbatim inline[/icode]
- [youtube]youtube link*[/youtube] (CURRENTLY RENDERS AS LINK!)
- [h1]big header[/h1]
- [h2]medium header[/h2]
- [h3]small header[/h3]
- [anchor=name]some text linkable with #name[/anchor]
- [spoiler=name]some text to hide[/spoiler]

And of course, the usual HTML characters are escaped everywhere: `', ", &, <, >`

**URLs not inside a url or img tag are auto-linked**, or at least a best attempt
is made at autolinking them (your mileage may vary)

## Caveats:

- Output removes \r and replaces \n with \<br\> (except in some tags with limited scope). If the \<br\>
  causes problems, you can use `BBCode::from_config(config, None)` and pass in a config with `newline_to_br` set
  to false
- Performance was not a main concern, although you can enable additional performance 
  features with the `perf` feature (enables some regex optimizations, about a 4x improvement in my
  testing)
- Many rules are arbitrary and meant to copy an existing bbcode parser I used for many years

## Changelog:

- **0.0.6**: Small bugfix for conditional compilation
- **0.1.0**: Full rewrite; if using `BBCode::default()`, or `BBCode::basics()` and `BBCode::extras()`,
  it should still compatible, but if you were creating custom tags at all, the entire system was
  scrapped in favor of the `ScopeInfo` and `EmitScope` combo
- **0.1.1**: Small bugfix to enforce Sync + Send on closures (so bbcode can be used across threads)
- **0.1.2**: Added class to "code" segments
- **0.1.3**: Added ability to convert a bbcode parser into one that only consumes the scoped tags it had
- **0.1.4**: Added secondary syntax I've seen around: `[tag tag=attribute]`
- **0.1.5**: Accidental upload ugh
- **0.1.6**: Added small configuration for link target (no API change, only new function)
- **0.1.7**: Consume some newlines around headers like most other bbcode parsers do
- **0.1.8**: Update onestop dependency
- **0.2.0**: Fix ScopeInfo visibility, change some defaults (\<br\> instead of newline), changed API 
  for parser setup
