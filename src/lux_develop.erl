%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2012-2020 Tail-f Systems AB
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(lux_develop).

-export([
         pre_markdown/2
        ]).

-include("lux.hrl").

pre_markdown(LuxAppDir, ToFile) ->
    FromFile = ToFile ++ ".src",
    AbsFileName0 = filename:absname(ToFile),
    AbsFileName = lux_utils:normalize_filename(AbsFileName0),
    RelFileName = lux_utils:drop_prefix(LuxAppDir, AbsFileName),
    RelFileDir = filename:dirname(RelFileName),
    {ok, Cwd} = file:get_cwd(),
    RelWorkDir = lux_utils:drop_prefix(LuxAppDir, Cwd),
    case file:read_file(FromFile) of
        {ok, Bin} ->
            try
                Expand =
                    fun(L, {Files, Lines}) ->
                            case markdown_line(RelWorkDir, L) of
                                {keep, Line} ->
                                    {Files, [Line | Lines]};
                                {expand, InclFile, InclLines} ->
                                    RelInclFile =
                                        relpath(RelFileDir, InclFile),
                                    {[RelInclFile | Files],
                                     [InclLines | Lines]};
                                {expand, EvalLines} ->
                                    {Files,
                                     [EvalLines | Lines]}
                            end
                    end,
                Lines = lux_utils:split_lines(Bin),
                {InclFiles, RevNewLines} =
                    lists:foldl(Expand, {[], []}, Lines),
                DepsIoList =
                    [
                     ToFile,
                     ": ",
                     join(" ", [FromFile | lists:reverse(InclFiles)])
                    ],
                DepsFile = ToFile ++ ".d",
                %% Make dependency file
                case file:write_file(DepsFile, DepsIoList) of
                    ok ->
                        ok;
                    {error, FileReason} ->
                        throw({error,
                               "~s: ~s", [DepsFile,
                                          file:format_error(FileReason)]})
                end,
                IoList = lux_utils:expand_lines(lists:reverse(RevNewLines)),
                %% Markdown file
                case file:write_file(ToFile, IoList) of
                    ok ->
                        ok;
                    {error, FileReason2} ->
                        throw({error,
                               "~s: ~s", [ToFile,
                                          file:format_error(FileReason2)]})
                end
            catch
                throw:{error, Format, Args} ->
                    ReasonStr = lists:flatten(io_lib:format(Format, Args)),
                    {error, FromFile, ReasonStr}
            end;
        {error, FileReason} ->
            {error, FromFile, file:format_error(FileReason)}
    end.

join(_Sep, []) ->
    [];
join(Sep, [H|T]) ->
 [H | join2(Sep, T)].

join2(_Sep, []) ->
    [];
join2(Sep, [H | T]) ->
    [Sep , H | join2(Sep, T)].

relpath(".", Path) ->
    Path;
relpath(Prefix, Path) ->
    UpDir = filename:join([".." || _ <- filename:split(Prefix)]),
    filename:join([UpDir, Path]).

markdown_line(RelWorkDir, Line) ->
    Incomplete = {error, "Incomplete include statement", []},
    case string:strip(binary_to_list(Line), left) of
        "#include " ++ InclFile ->
            case string:strip(InclFile, both) of
                "<" ->
                    throw(Incomplete);
                "<" ++ InclFile2 ->
                    case lists:reverse(InclFile2) of
                        ">" ++ RevInclFile ->
                            InclFile3 = lists:reverse(RevInclFile),
                            markup_include(InclFile3);
                        _ ->
                            throw(Incomplete)
                    end;
                _ ->
                    throw(Incomplete)
            end;
        "#eval-include " ++ Cmd0 ->
            Cmd = string:strip(Cmd0),
            FullCmd =
                case relpath(RelWorkDir, "") of
                    "" -> Cmd;
                    UpDir  -> "cd " ++ UpDir ++ " && " ++ Cmd
                end,
            io:format("\t~s\n", [FullCmd]),
            {Output, RetCode} = lux_utils:cmd(FullCmd),
            io:format("\techo $?\n\t~s\n", [RetCode]),
            Prompt = ".../lux> ",
            Prefix = ">     ",
            {expand,
             [
              "Evaluate `", Cmd, "`\n\n",
              Prefix, Prompt, Cmd, "\n",
              lux_utils:expand_lines([[Prefix, L] || L <- Output]),
              "\n",
              Prefix, Prompt, "echo $?\n",
              Prefix, RetCode, "\n"
             ]};
        "#eval-silent " ++ Cmd0 ->
            Cmd = string:strip(Cmd0),
            FullCmd =
                case relpath(RelWorkDir, "") of
                    "" -> Cmd;
                    UpDir  -> "cd " ++ UpDir ++ " && " ++ Cmd
                end,
            io:format("\t~s\n", [FullCmd]),
            {_Output, RetCode} = lux_utils:cmd(FullCmd),
            io:format("\techo $?\n\t~s\n", [RetCode]),
            {expand,
             [
              "Evaluate `", Cmd, "`\n"
             ]};
        _ ->
            {keep, Line}
    end.

markup_include(RelFile) ->
    AbsFile = filename:join(["..", RelFile]),
    case file:read_file(AbsFile) of
        {ok, Bin} ->
            Lines = lux_utils:split_lines(binary_to_list(Bin)),
            RelFile2 = filename:join(["...", "lux", RelFile]),
            {expand,
             RelFile,
             [
              "Snippet from the enclosed `", RelFile2, "` file:\n",
              "\n",
              lux_utils:expand_lines([[">     ", L] || L <- Lines])
             ]};
        {error, FileReason} ->
            RelFile2 = filename:join(["..", "lux", RelFile]),
            throw({error, "~s: ~s", [RelFile2, file:format_error(FileReason)]})
    end.
