# More BBCode parsers?

Yeah! I needed something highly extensible, flexible, and specifically WITH scoping 
rules so it always produces correct HTML. For instance, there's stuff like:

```
[b]This is bold! [i]AND ITALIC?[/b] oops where's [/i]?
```

Where you have unmatching closing tags. While a simple regex replacement may handle this
any number of ways, this library will produce:

```html
<b>This is bold! <i>AND ITALIC?</i></b> oops where&#39;s ?
```

Another example:

```
And so [s] I'm just like [s] opening tons of [sup] tags? Maybe I'll close this [/s] one
```

```html
And so <s> I&#39;m just like <s> opening tons of <sup> tags? Maybe I&#39;ll close this </sup></s> one</s>
```

All unclosed tags are automatically closed in the correct order, including any that were
left at the end of the text. Unmatched closing tags are removed. Of course, this may not
be what you want, but I've found that most older or established bbcode parsers work this
way. With this library, you can feel (generally) safe knowing it will produce proper HTML.

With scoping rules, you also get access to tags which can reject other tags inside of them,
for "verbatim" sections. For instance, in the extended tagset, I have [code] which

## Quickstart 

```rust
let bbcode = BBCode::default().unwrap(); //Or catch error
let html = bbcode.parse("[url]https://github.com[/url]")
```

Or, if you want the extended set (see next section for list):

```rust
// These are just vectors, which means you can construct your own!
let mut matchers = BBCode::basics().unwrap();
let mut extras = BBCode::extras().unwrap();
matchers.append(&mut extras);

//Note: this step could be expensive, as it has to compile a couple dozen regexes
let bbcode = BBCode::from_matchers(matchers);

// Cheap copy: they share the same pre-compiled regex, share it around!
let bbcode2 = bbcode.clone();
```

Or, if you want to add your own tag:

```rust
//Version 1 is ugly, will probably add helper functions
let mut matchers = BBCode::basics().unwrap();
let tags = vec![
    //These were ommitted but you can add them back in easily!
    TagInfo::simple("center"),
    TagInfo::simple("left"),
    TagInfo::simple("right"),
];
let mut mytags = BBCode::tags_to_matches(tags).unwrap();
matchers.append(&mut mytags);
let bbcode = BBCode::from_matchers(matchers);

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
- [list][*]item[*]item2[/list]

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

And of course, the usual HTML characters are escaped: `', ", &, <, >`

**URLs not inside a url or img tag are auto-linked**, or at least a best attempt
is made at autolinking them (your mileage may vary)

## Caveats:

- Output removes \r but RETAINS \n rather than replacing with <br>. This was how an old
  bbcode parser I was using worked, and this was written to replace that. If there's a need,
  I can add modes for \n vs <br>
- Performance was not a main concern, although you can enable additional performance 
  features from the regex crate with the `perf` feature
- Many rules are arbitrary and meant to copy an existing bbcode parser I used for many years

## Future

I mostly published this for my own projects, which have specific requirements, but if for 
some reason this gets picked up and used and there are gaps or bugs or other required
features, I'd be willing to work on it! I just don't see that happening lol
