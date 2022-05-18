%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Logging helpers for PropEr and Common Test
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration =====================================================
-module(trainee_test_logging).

%%%_* Exports ================================================================
%%%_ * API -------------------------------------------------------------------
-export([
    log/4,
    log/5,
    log/7,
    log/8,
    log_call/5,
    log_call/6,
    log_call/7,
    log_error/4,
    log_error/5,
    log_error/8,
    log_postcondition_failed/8,
    log_proper_error/4,
    log_proper_results/6
]).

-export([
    format/2,
    format/4,
    format/7,
    format_stack/1,
    format_stack/2
]).

%%%_* Types ------------------------------------------------------------------

%%%_* Includes ===============================================================
-include("internal.hrl").

%%%_* Macros =================================================================
%% Matches the `pattern' of `?assertException'
-define(ASSERT_EXCEPTION_PATTERN_REGEXP,
    "^\{\s*([a-z]+)\s*,\s*(.+?)\s*,\s*[[]\.{3}[]]\s*\}$"
).

%% ANSI escape code for dimmed test
-define(DIM, "\e[2;39m").

%% ANSI escape code for reseting to normal, i.e. unformatted, text
-define(RESET, "\e[0m").

%% Maximum recursion for formatting terms to the terminal, see {@link io:format/2} `~P'.
-define(MAX_DEPTH, 20).

%%%_* Types ==================================================================
-type file() :: string().
-type line() :: pos_integer().

%%%_* Code ===============================================================
%%%_ * API -------------------------------------------------------------------

%% @doc Like {@link io:format/2} but accepts an {@link iolist()} as formatting
%% string, and guaranteed to always return a flatten string.
format(FormatString, Arguments) ->
    lists:flatten(io_lib:format(lists:flatten(FormatString), Arguments)).

%% @doc Formats the given arguments according to the given formatting string
%% with the given file and line appended into both HTML and terminal friendly
%% format.
%%
%% You can give it a single formatting string a pair of separate HTML and
%% terminal formatting strings. In addition you may give either a single list
%% of arguments to format, or separate HTML and terminal arguments lists.
format(
    {TerminalFormat, HTMLFormat},
    {TerminalArguments, HTMLArguments},
    File,
    Line
) ->
    Terminal = io_lib:format(TerminalFormat, TerminalArguments),
    HTML = format_and_escape(HTMLFormat, HTMLArguments),
    {
        [
            Terminal,
            " ",
            format_location(terminal, File, Line)
        ],
        [
            HTML,
            " ",
            "<span class=\"log-location\">",
            format_location(html, File, Line),
            "</span>"
        ]
    };
format({TerminalFormat, HTMLFormat}, Arguments, File, Line) ->
    format({TerminalFormat, HTMLFormat}, {Arguments, Arguments}, File, Line);
format(Format, Arguments, File, Line) ->
    format({Format, Format}, {Arguments, Arguments}, File, Line).

%% @doc Like {@link format/4} but also accepts a module, function, arity
%% triple to be appended along the file and line.
format(Format, Arguments, Module, Function, Arity, File, Line) when is_integer(Arity) ->
    ArityString = io_lib:format("/~p", [Arity]),
    format_internal(
        Format, Arguments, Module, Function, ArityString, ArityString, File, Line
    );
format(Format, Arguments, Module, Function, FunctionArguments, File, Line) when
    is_list(FunctionArguments)
->
    TerminalArguments = [
        format("~tp", [Argument])
     || Argument <- FunctionArguments
    ],
    HTMLArguments = [
        format_and_escape("~tp", [Argument])
     || Argument <- FunctionArguments
    ],
    TerminalArity = ["(", lists:join(", ", TerminalArguments), ")"],
    HTMLArity = ["(", lists:join(", ", HTMLArguments), ")"],
    format_internal(
        Format, Arguments, Module, Function, TerminalArity, HTMLArity, File, Line
    ).

%% @doc Like {@link format_stack/2} with a `terminal' output type.
-spec format_stack([Frame]) -> iolist() when
    Frame :: {module(), FunctionName, ArityOrArguments, Location},
    FunctionName :: atom(),
    ArityOrArguments :: arity() | [term()],
    Location :: [{file, file()} | {line, line()}].
format_stack(Stacktrace) ->
    format_stack(terminal, Stacktrace).

%% @doc Formats the given stacktrace into something nice.
%% The default stacktrace is just a list of four-tuples, which is a bit hard
%% to digest quickly.
%%
%% Instead this formats like `mod:fun/1 in file:line' optionally appended
%% with the actual arguments if available (formated using `~tp').
%% If outputting to the terminal the `in' is dimmed to make it easier to
%% differentiate between the MFA and file:line parts.
%%
%% If outputting HTML each file:line is rendered as an anchor tag pointing to
%% your favorite editor (using the `EDITOR' OS environmental variable).
-spec format_stack(OutputType, [Frame]) -> iolist() when
    OutputType :: terminal | html,
    Frame :: {module(), FunctionName, ArityOrArguments, Location},
    FunctionName :: atom(),
    ArityOrArguments :: arity() | [term()],
    Location :: [{file, file()} | {line, line()}].
format_stack(OutputType, Stacktrace) ->
    [
        format_frame(OutputType, Module, Function, ArityOrArguments, Location)
     || {Module, Function, ArityOrArguments, Location} <- Stacktrace
    ].

%% @doc Formats the given list of terms according to the given formatting
%% string using {@link format/4} and logs it using {@link ct:print2} and
%% {@link ct:log/2}.
log(Format, Arguments, File, Line) ->
    {Terminal, HTML} = format(Format, Arguments, File, Line),
    ct:print("~s", [Terminal]),
    ct:log("~s", [HTML]).

%% @doc Like {@link log/4}, but accepts and additional `Type', used as the
%% category when logging using {@link ct:log/3}.
log(Type, Format, Arguments, File, Line) ->
    {Terminal, HTML} = format(Format, Arguments, File, Line),
    ct:print("~s", [Terminal]),
    ct:log(Type, "~s", [HTML]).

%% @doc Like {@link log/4}, but accepts a module, function, arity triple.
%%
%% @see format/7
log(Format, Arguments, Module, Function, Arity, File, Line) ->
    {Terminal, HTML} = format(
        Format, Arguments, Module, Function, Arity, File, Line
    ),
    ct:print("~s", [Terminal]),
    ct:log("~s", [HTML]).

%% @doc Like {@link log/5}, but accepts a module, function, arity triple.
%%
%% @see format/7
log(Type, Format, Arguments, Module, Function, Arity, File, Line) ->
    {Terminal, HTML} = format(
        Format, Arguments, Module, Function, Arity, File, Line
    ),
    ct:print("~s", [Terminal]),
    ct:log(Type, "~s", [HTML]).

%% @doc Logs a function call using {@link ct:print/2} and {@link ct:log/3}.
%%
%% The part logged to {link ct. CT} HTML report will have a `call' and
%% `Function' CSS class for easy styling.
log_call(Module, Function, Arguments, File, Line) ->
    CSSClass = list_to_atom("call " ++ atom_to_list(Function)),
    {Terminal, HTML} = format(
        "",
        [],
        Module,
        Function,
        Arguments,
        File,
        Line
    ),
    ct:print("~s", [Terminal]),
    ct:log(CSSClass, "~s", [HTML]).

%% @doc Like {@link log_call/5} but also log the return value of the call.
log_call(Module, Function, Arguments, Return, File, Line) when is_list(Arguments) ->
    CSSClass = list_to_atom("call " ++ atom_to_list(Function)),
    {Terminal, HTML} = format(
        "",
        [],
        Module,
        Function,
        Arguments,
        File,
        Line
    ),
    ct:print("~s\n->\n~tp", [Terminal, Return]),
    ct:log(CSSClass, "~s\n->\n~s\n", [HTML, term_to_html(Return)]),
    Return;
log_call(Type, Module, Function, Arguments, File, Line) ->
    CSSClass = list_to_atom(
        lists:flatten(
            ["call ", atom_to_list(Type), " ", atom_to_list(Function)]
        )
    ),
    {Terminal, HTML} = format(
        "",
        [],
        Module,
        Function,
        Arguments,
        File,
        Line
    ),
    ct:print("~s", [Terminal]),
    ct:log(CSSClass, "~s", [HTML]).

%% @doc Like {@link log_call/6} with an extra category/CSS class appended.
log_call(Type, Module, Function, Arguments, Return, File, Line) ->
    CSSClass = list_to_atom(
        lists:flatten(
            ["call ", atom_to_list(Type), " ", atom_to_list(Function)]
        )
    ),
    {Terminal, HTML} = format(
        "",
        [],
        Module,
        Function,
        Arguments,
        File,
        Line
    ),
    ct:print("~s\n->\n~tp", [Terminal, Return]),
    ct:log(CSSClass, "~s\n->\n~s\n", [HTML, term_to_html(Return)]),
    Return.

%% @doc Logs an error using {@link ct:print/2} and {@link ct:log/3}.
%%
%% The part logged to {link ct. CT} HTML report will have a `Class' CSS class
%% for easy styling.
log_error(Class, Reason, Stacktrace, Message) ->
    log_error('', Class, Reason, Stacktrace, Message).

%% @doc Like {@link log_error/4}, but with a MFA, file and line appended.
log_error(Class, Reason, Stacktrace, Module, Function, Arguments, File, Line) ->
    log_error(
        Class,
        Reason,
        Stacktrace,
        format(
            "An error occurred",
            [],
            Module,
            Function,
            Arguments,
            File,
            Line
        )
    ).

%% @doc Like {@link log_error/4} with an extra category/CSS class appended.
log_error(Type, Class, Reason, Stacktrace, {Terminal, HTML}) ->
    CSSClass = list_to_atom("error " ++ atom_to_list(Type)),
    ct:print(
        "~s\n\n~s(~tp)\n~s",
        [
            Terminal,
            Class,
            Reason,
            format_stack(terminal, Stacktrace)
        ]
    ),
    ct:log(
        CSSClass,
        "~s\n\n~s(~s)\n<hr />~s\n",
        [
            HTML,
            Class,
            term_to_html(Reason),
            format_stack(html, Stacktrace)
        ]
    );
log_error(Type, Class, Reason, Stacktrace, Message) ->
    log_error(Type, Class, Reason, Stacktrace, {Message, Message}).
log_proper_results(Commands, History, DynamicState, Result, File, Line) ->
    % Module = get_state_machine_under_test(),
    case simplify_proper_result(Result) of
        {ok, Message} ->
            log('proper success', "~s\n~tp\n", [Message, DynamicState], File, Line);
        {error, Error, Exception} ->
            Message = [
                Error,
                $\n,
                format_proper_command_history(Commands, History)
            ],
            log_proper_error(Message, Exception, File, Line)
    end,
    ok.

log_proper_error(
    Message, {exception, error, Reason = {AssertType, InfoList}, Stacktrace}, File, Line
) when
    ?is_assert_error(Reason)
->
    Info = maps:from_list(InfoList),
    MaybeComment =
        case Info of
            #{comment := Comment} ->
                [<<" with ">>, Comment];
            _ ->
                ""
        end,
    log_error(
        proper,
        error,
        format_assert_error(AssertType, Info),
        Stacktrace,
        format(
            "~s~s",
            [Message, MaybeComment],
            File,
            Line
        )
    );
log_proper_error(Message, {exception, Class, Reason, Stacktrace}, File, Line) ->
    log_error(
        proper,
        Class,
        Reason,
        Stacktrace,
        format(
            "~s",
            [Message],
            File,
            Line
        )
    );
log_proper_error(Message, false, File, Line) ->
    {current_stacktrace, Stacktrace} = erlang:process_info(self(), current_stacktrace),
    log_error(
        proper,
        error,
        false,
        Stacktrace,
        format(
            "~s",
            [Message],
            File,
            Line
        )
    ).

log_postcondition_failed(
    {AssertType, InfoList}, Module, Function, Arguments, Result, File, Line, Stacktrace
) ->
    Info = maps:from_list(InfoList),
    {MessageFormat, MessageArguments} = format_assert_error(AssertType, Info),
    {Terminal, HTML} = format(
        "Postcondition failed with ~s\n" ++ MessageFormat ++ "\n",
        [neo:get(Info, comment, "") | MessageArguments],
        Module,
        Function,
        Arguments,
        File,
        Line
    ),
    CSSClass = list_to_atom("proper error " ++ atom_to_list(AssertType)),
    ct:print("~s->\n~tp\n~s", [Terminal, Result, format_stack(terminal, Stacktrace)]),
    ct:log(CSSClass, "~s->\n~s\n<hr />~s\n", [
        HTML,
        term_to_html(Result),
        format_stack(html, Stacktrace)
    ]).

%%%_* Private ----------------------------------------------------------------
format_and_escape(FormatString, Arguments) ->
    xmerl_lib:export_text(format(FormatString, Arguments)).

term_to_html(Term) ->
    format_and_escape("~tp", [Term]).

format_internal(
    {TerminalFormat, HTMLFormat},
    {TerminalArguments, HTMLArguments},
    Module,
    Function,
    TerminalArity,
    HTMLArity,
    File,
    Line
) ->
    {
        lists:join(" ", [
            io_lib:format(TerminalFormat, TerminalArguments),
            io_lib:format("~s:~s~s", [Module, Function, TerminalArity]),
            dim_word(terminal, "in"),
            format_location(terminal, File, Line)
        ]),
        lists:join(" ", [
            format_and_escape(HTMLFormat, HTMLArguments),
            "<span class=\"log-mfa\">",
            io_lib:format("~s:~s~s", [Module, Function, HTMLArity]),
            "</span>",
            "<span class=\"log-location\">",
            dim_word(html, "in"),
            format_location(html, File, Line),
            "</span>"
        ])
    };
format_internal(
    {TerminalFormat, HTMLFormat},
    Arguments,
    Module,
    Function,
    TerminalArity,
    HTMLArity,
    File,
    Line
) ->
    format_internal(
        {TerminalFormat, HTMLFormat},
        {Arguments, Arguments},
        Module,
        Function,
        TerminalArity,
        HTMLArity,
        File,
        Line
    );
format_internal(
    Format,
    Arguments,
    Module,
    Function,
    TerminalArity,
    HTMLArity,
    File,
    Line
) ->
    format_internal(
        {Format, Format},
        {Arguments, Arguments},
        Module,
        Function,
        TerminalArity,
        HTMLArity,
        File,
        Line
    ).

%% @doc Dimmes the given word according to the given output type.
dim_word(terminal, Word) ->
    [?DIM, Word, ?RESET];
dim_word(html, Word) ->
    Word.

%% @private
format_frame(OutputType, Module, Function, Arity, Location) when is_integer(Arity) ->
    %% mod:fun/1 in file:line
    {File, Line} = location(Location),
    io_lib:format("~s:~s/~p ~s ~s~n", [
        Module,
        Function,
        Arity,
        dim_word(OutputType, "in"),
        format_location(OutputType, File, Line)
    ]);
format_frame(terminal, Module, Function, Arguments, Location) ->
    %% mod:fun/1 with [args] in file:line
    {File, Line} = location(Location),
    io_lib:format("~s:~s/~p ~s ~s ~s ~n~tW~n", [
        Module,
        Function,
        length(Arguments),
        dim_word(terminal, "in"),
        format_location(terminal, File, Line),
        dim_word(terminal, "with"),
        Arguments,
        ?MAX_DEPTH
    ]);
format_frame(html, Module, Function, Arguments, Location) ->
    %% mod:fun/1 with [args] in file:line
    {File, Line} = location(Location),
    io_lib:format("~s:~s/~p in ~s with ~n~s~n", [
        Module,
        Function,
        length(Arguments),
        format_location(html, File, Line),
        term_to_html(Arguments)
    ]).

%% @doc Formats the given source code location according to the given output
%% type.
-spec format_location(html | terminal, file(), line()) -> iolist().
format_location(_, undefined, _) -> "";
format_location(terminal, File, undefined) -> File;
format_location(terminal, File, Line) -> io_lib:format("~s:~p", [File, Line]);
format_location(html, File, Line) -> format_anchor(File, Line).

format_anchor(File, Line) ->
    format_anchor(os:getenv("EDITOR"), File, Line).

format_anchor("code", File, Line) ->
    %% VS Code
    FileWithLine = format_location(terminal, File, Line),
    anchor("vscode://file/", FileWithLine, FileWithLine);
format_anchor("emacs", File, Line) ->
    %% Emacs
    FileWithLine = format_location(terminal, File, Line),
    URI = percent_encode(FileWithLine),
    anchor("org-protocol://open-source://", URI, FileWithLine);
format_anchor("mvim", File, undefined) ->
    %% MacVim
    Query = uri_string:compose_query(
        [{"url", percent_encode("file://" ++ File)}]
    ),
    anchor("mvim://open?", Query, File);
format_anchor("mvim", File, Line) ->
    %% MacVim
    Query = uri_string:compose_query(
        [
            {"url", percent_encode("file://" ++ File)},
            {"line", Line}
        ]
    ),
    anchor("mvim://open?", Query, File);
format_anchor(_Editor, File, Line) ->
    format_location(terminal, File, Line).

%% @private Unfortunately erl_anno is lying about it's type signature.
-dialyzer({nowarn_function, line/1}).
-spec line(erl_anno:anno() | proplists:proplist() | erl_syntax:syntaxTree()) ->
    non_neg_integer() | undefined.
line(LocationOrNode) ->
    Location =
        try erl_syntax:type(LocationOrNode) of
            _ ->
                erl_syntax:get_pos(LocationOrNode)
        catch
            _:_ ->
                LocationOrNode
        end,
    case erl_anno:line(Location) of
        undefined -> proplists:get_value(line, Location);
        L -> L
    end.

%% @private Dialyzer does not like us looking into the internal representation
%% of erl_anno, even though the code uses erl_anno:File/1.
-dialyzer({nowarn_function, file/1}).
-spec file(erl_anno:anno() | proplists:proplist() | erl_syntax:syntaxTree()) ->
    string() | undefined.
file(LocationOrNode) ->
    Location =
        try erl_syntax:type(LocationOrNode) of
            _ ->
                erl_syntax:get_pos(LocationOrNode)
        catch
            _:_ ->
                LocationOrNode
        end,
    case erl_anno:file(Location) of
        undefined -> proplists:get_value(file, Location);
        F -> F
    end.

%% @private
location(Location) ->
    {file(Location), line(Location)}.

%% @private
anchor(Protocol, URI, FileMaybeWithLine) ->
    ["<a href=\"", Protocol, URI, "\">", FileMaybeWithLine, "</a>"].

%% @private
%% @doc Turns out there's no pure percent encoding function in the stdlib.
%% But there is an implementation hidden inside `uri_string'.
percent_encode(Text) ->
    "=" ++ URI = uri_string:compose_query(
        [{"", Text}], [{encoding, latin1}]
    ),
    URI.

simplify_proper_result(ok) ->
    {ok, "Proper succeeded"};
simplify_proper_result(initialization_error) ->
    {error, "Initialization error", false};
simplify_proper_result(no_possible_interleaving) ->
    {error, "No possible interleaving", false};
simplify_proper_result({precondition, Exception}) ->
    {error, "Precondition failed", Exception};
simplify_proper_result({precondition, false, Exception}) ->
    {error, "Precondition failed", Exception};
simplify_proper_result({postcondition, Exception}) ->
    {error, "Postcondition failed", Exception};
simplify_proper_result({postcondition, false, Exception}) ->
    {error, "Postcondition failed", Exception};
simplify_proper_result(Exception) ->
    {error, "Exception", Exception}.

format_proper_command_history(Commands, History) ->
    {EvaluatedCommands, NonEvaluatedCommands} = lists:split(length(History), Commands),
    [
        lists:join(<<",\n">>, [
            io_lib:format("DynamicState~tp = ~s,\n~s\n     = ~tp", [
                N,
                format_proper_symbolic_call_argument(1, DynamicState),
                format_proper_command(1, Command),
                Result
            ])
         || {Command, {DynamicState, Result}, N} <- lists:zip3(
                EvaluatedCommands, History, lists:seq(1, length(History))
            )
        ]),
        case NonEvaluatedCommands of
            [] ->
                "";
            _ ->
                [
                    <<"\n\n%% Not evaluated commands\n">>,
                    mapjoin(
                        1, <<",\n">>, fun format_proper_command/2, NonEvaluatedCommands
                    )
                ]
        end,
        <<".">>
    ].

mapjoin(Indent, Separator, Fun, List) when
    is_integer(Indent) andalso Indent >= 0 andalso is_function(Fun, 2) andalso
        is_list(List)
->
    lists:join(Separator, [Fun(Indent, Value) || Value <- List]).

format_proper_command(Indent, {set, {var, Name}, {call, Module, Function, Arguments}}) ->
    Call = format_proper_symbolic_call(Indent, Module, Function, Arguments),
    Var = format_symbolic_variable(Name),
    io_lib:format("~s = ~s", [Var, string:trim(Call, leading)]).

format_symbolic_variable(N) when is_integer(N) ->
    io_lib:format("Var~tp", [N]);
format_symbolic_variable(Name) ->
    io_lib:format("~s", [Name]).

format_proper_symbolic_call(Indent, Module, Function, Arguments) ->
    [
        lists:duplicate(Indent * 4, <<" ">>),
        io_lib:format("~s:~s(", [Module, Function]),
        mapjoin(
            Indent, <<", ">>, fun format_proper_symbolic_call_argument/2, Arguments
        ),
        <<")">>
    ].

format_proper_symbolic_call_argument(_Indent, {var, Name}) ->
    format_symbolic_variable(Name);
format_proper_symbolic_call_argument(Indent, {call, Module, Function, Arguments}) ->
    format_proper_symbolic_call(Indent, Module, Function, Arguments);
format_proper_symbolic_call_argument(Indent, List) when is_list(List) ->
    [
        <<"[">>,
        mapjoin(Indent, <<", ">>, fun format_proper_symbolic_call_argument/2, List),
        <<"]">>
    ];
format_proper_symbolic_call_argument(Indent, Tuple) when is_tuple(Tuple) ->
    [
        <<"{">>,
        mapjoin(
            Indent,
            <<", ">>,
            fun format_proper_symbolic_call_argument/2,
            tuple_to_list(Tuple)
        ),
        <<"}">>
    ];
format_proper_symbolic_call_argument(Indent, Map) when is_map(Map) ->
    LineBreak = [<<",\n">>, lists:duplicate(Indent * 4, <<" ">>)],
    io_lib:format("#{~*.1c~s~*.1c}", [
        Indent * -4,
        $\n,
        lists:join(LineBreak, [
            [
                format_proper_symbolic_call_argument(Indent + 1, Key),
                <<" => ">>,
                string:trim(
                    format_proper_symbolic_call_argument(Indent + 1, Value), leading
                )
            ]
         || {Key, Value} <- maps:to_list(Map)
        ]),
        Indent * -4,
        $\n
    ]);
format_proper_symbolic_call_argument(_Indent, Value) ->
    io_lib:format("~tp", [Value]).

format_assert_error(assert, #{expression := Expression, expected := Expected} = Info) ->
    Actual =
        case Info of
            #{value := Value} -> Value;
            #{not_boolean := Value} -> Value
        end,
    MaybeNot =
        case Expected of
            true -> "";
            false -> "not "
        end,
    {"Expected ~s~s, got ~tp instead", [MaybeNot, Expression, Actual]};
format_assert_error(assertMatch, #{
    expression := Expression, pattern := Pattern, value := Actual
}) ->
    {"Expected ~s with value ~tp to match ~s", [Expression, Actual, Pattern]};
format_assert_error(assertNotMatch, #{
    expression := Expression, pattern := Pattern, value := Actual
}) ->
    {"Expected ~s with value ~tp to not match ~s", [Expression, Actual, Pattern]};
format_assert_error(assertEqual, #{
    expression := Expression, expected := Expected, value := Actual
}) ->
    {"Expected ~tp =:= ~s, got ~tp instead", [Expected, Expression, Actual]};
format_assert_error(assertNotEqual, #{
    expression := Expression, expected := Expected, value := Actual
}) ->
    {"Expected ~tp =/= ~s, got ~tp instead", [Expected, Expression, Actual]};
format_assert_error(assertExpection, #{
    expression := Expression, pattern := Pattern, unexpected_success := Value
}) ->
    case re:run(Pattern, ?ASSERT_EXCEPTION_PATTERN_REGEXP) of
        {match, [Class, Reason]} ->
            {"Expected ~tp to raise ~s(~s), got ~tp instead", [
                Expression, Class, Reason, Value
            ]};
        nomatch ->
            {"Expected ~tp to raise ~s, got ~tp instead", [Expression, Pattern, Value]}
    end;
format_assert_error(assertExpection, #{
    expression := Expression,
    pattern := Pattern,
    unexpected_exception := {Class, Reason, _Stacktrace}
}) ->
    case re:run(Pattern, ?ASSERT_EXCEPTION_PATTERN_REGEXP) of
        {match, [ExpectedClass, ExpectedReason]} ->
            {"Expected ~tp to raise ~s(~s), instead it raised ~s(~tp)", [
                Expression,
                ExpectedClass,
                ExpectedReason,
                Class,
                Reason
            ]};
        nomatch ->
            {"Expected ~tp to raise ~s, instead it raised ~s(~tp)", [
                Expression, Pattern, Class, Reason
            ]}
    end;
format_assert_error(assertNotException, #{
    expression := Expression,
    pattern := Pattern,
    unexpected_exception := {Class, Reason, _Stacktrace}
}) ->
    case re:run(Pattern, ?ASSERT_EXCEPTION_PATTERN_REGEXP) of
        {match, [ExpectedClass, ExpectedReason]} ->
            {"Expected ~tp to not raise ~s(~s), instead it raised ~s(~tp)", [
                Expression,
                ExpectedClass,
                ExpectedReason,
                Class,
                Reason
            ]};
        nomatch ->
            {"Expected ~tp to not raise ~s, instead it raised ~s(~tp)", [
                Expression, Pattern, Class, Reason
            ]}
    end.

%%%_* Tests ==================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
