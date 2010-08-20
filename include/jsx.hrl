%% The MIT License

%% Copyright (c) 2010 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-include("jsx_common.hrl").



%% opts record
-record(opts, {
    comments = false,
    escaped_unicode = codepoint,
    multi_term = false,
    encoding = auto
}).


-spec parser() -> jsx_parser().
-spec parser(Opts::jsx_opts()) -> jsx_parser().

-spec term_to_json(JSON::eep0018()) -> binary().
-spec term_to_json(JSON::eep0018(), Opts::encoder_opts()) -> binary().

-spec json_to_term(JSON::binary()) -> eep0018().
-spec json_to_term(JSON::binary(), Opts::decoder_opts()) -> eep0018().    

-spec is_json(JSON::binary()) -> true | false.
-spec is_json(JSON::binary(), Opts::verify_opts()) -> true | false.

-spec format(JSON::binary()) -> binary() | iolist().
-spec format(JSON::binary(), Opts::format_opts()) -> binary() | iolist().

-spec eventify(List::list()) -> jsx_parser_result().
