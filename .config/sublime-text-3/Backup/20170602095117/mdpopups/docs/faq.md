# F.A.Q. {: .doctitle}
Frequently asked questions.

---

# Questions

- **Why don't `#!html <pre>` tags work right?**

    This is because the HTML engine in Sublime treats `#!html <pre>` tags just as a normal block element; it doesn't treat the content as preformatted.  When MdPopups creates code blocks, it actually specially formats the blocks.  It converts tabs to 4 spaces, any contiguous spaces after the first are converted to `#!html &nbsp;`.  And lastly, new lines get converted to `#!html <br>` tags.
    {: style="font-style: italic"}

- **Why in code blocks do tabs get converted to 4 spaces?**

    Because I like it that way.  If you are planning on having a snippet of text sent through the syntax highlighter and do not want your tabs to be converted to 4 spaces, you should convert it to the number of spaces you like **before** sending it through the syntax highlighter.
    {: style="font-style: italic"}

- **Why does &lt;insert element&gt; not work, or cause the tooltip not to show?**

    Because Sublime's HTML engine is extremely limited.  Though I do not have a complete list of all supported elements, you should keep things basic.  Things like `#!html <table>` will not work.
    {: style="font-style: italic"}

- **Why can't I get CSS selectors like this to work: `#!css div p { ... }`?**

    This now work in Sublime Text build 3119+, but read on if you are below that build. Sublime's CSS handling is very limited and cannot handle parent and child selectors etc.  It handles one element at a time only.  You can chain classes: `#!css .myclass.myotherclass`, but you **cannot** have a parent and a child class selector or elementes: `#!css .myclass .myotherclass`.
    {: style="font-style: italic"}

- **I tried pushing a massive amount of HTML through the tooltip and Sublime crashed.  What do I do?**

    This may be more stable now, but I will leave this here just in case.  I won't bother getting into the fact that technically Sublime should handle these situation gracefully, so I'll just say that the tooltip API has some limitations; use it reasonably.
    {: style="font-style: italic"}
