" ============================================================================
" File:        litecorrect.vim
" Description: autoload functions for vim-litecorrect plugin
" Maintainer:  Reed Esau <github.com/reedes>
" Created:     January 20, 2014
" License:     The MIT License (MIT)
" ============================================================================

scriptencoding utf-8

if exists("autoloaded_litecorrect")
  finish
endif
let autoloaded_litecorrect = 1

function! s:unicode_enabled()
  return &encoding == 'utf-8'
endfunction

function! litecorrect#init(...)

  ia <buffer> Iam I am
  ia <buffer> Im I'm
  ia <buffer> TEh The
  ia <buffer> THat That
  ia <buffer> THe The
  ia <buffer> Teh The
  ia <buffer> abotu about
  ia <buffer> acn can
  ia <buffer> adn and
  ia <buffer> aer are
  ia <buffer> agian again
  ia <buffer> ahev have
  ia <buffer> ahve have
  ia <buffer> alos also
  ia <buffer> alot a lot
  ia <buffer> alse else
  ia <buffer> alsot also
  ia <buffer> amde made
  ia <buffer> amke make
  ia <buffer> amkes makes
  ia <buffer> anbd and
  ia <buffer> andd and
  ia <buffer> anf and
  ia <buffer> ans and
  ia <buffer> aobut about
  ia <buffer> aslo also
  ia <buffer> asthe as the
  ia <buffer> atthe at the
  ia <buffer> awya away
  ia <buffer> aywa away
  ia <buffer> bakc back
  ia <buffer> baout about
  ia <buffer> bcak back
  ia <buffer> beacuse because
  ia <buffer> becuase because
  ia <buffer> bve be
  ia <buffer> cant can't
  ia <buffer> chaneg change
  ia <buffer> chanegs changes
  ia <buffer> chekc check
  ia <buffer> chnage change
  ia <buffer> chnaged changed
  ia <buffer> chnages changes
  ia <buffer> claer clear
  ia <buffer> cmo com
  ia <buffer> cna can
  ia <buffer> coudl could
  ia <buffer> cpoy copy
  ia <buffer> dael deal
  ia <buffer> didnot did not
  ia <buffer> didnt didn't
  ia <buffer> diea idea
  ia <buffer> doens does
  ia <buffer> doese does
  ia <buffer> doesnt doesn't
  ia <buffer> doign doing
  ia <buffer> doimg doing
  ia <buffer> donig doing
  ia <buffer> dont don't
  ia <buffer> eahc each
  ia <buffer> efel feel
  ia <buffer> ehlp help
  ia <buffer> ehr her
  ia <buffer> emial email
  ia <buffer> ened need
  ia <buffer> enxt next
  ia <buffer> esle else
  ia <buffer> ew we
  ia <buffer> eyar year
  ia <buffer> eyt yet
  ia <buffer> fatc fact
  ia <buffer> fidn find
  ia <buffer> fiel file
  ia <buffer> firts first
  ia <buffer> flase false
  ia <buffer> fo of
  ia <buffer> fomr form
  ia <buffer> fora for a
  ia <buffer> forthe for the
  ia <buffer> foudn found
  ia <buffer> frmo from
  ia <buffer> fro for
  ia <buffer> frome from
  ia <buffer> fromthe from the
  ia <buffer> fwe few
  ia <buffer> gerat great
  ia <buffer> gievn given
  ia <buffer> goign going
  ia <buffer> gonig going
  ia <buffer> gruop group
  ia <buffer> grwo grow
  ia <buffer> haev have
  ia <buffer> hasa has a
  ia <buffer> havea have a
  ia <buffer> hda had
  ia <buffer> hge he
  ia <buffer> hlep help
  ia <buffer> holf hold
  ia <buffer> hsa has
  ia <buffer> hsi his
  ia <buffer> htat that
  ia <buffer> hte the
  ia <buffer> htem them
  ia <buffer> hten then
  ia <buffer> htere there
  ia <buffer> htese these
  ia <buffer> htey they
  ia <buffer> hting thing
  ia <buffer> htink think
  ia <buffer> htis this
  ia <buffer> hvae have
  ia <buffer> hvaing having
  ia <buffer> hvea have
  ia <buffer> hwich which
  ia <buffer> idae idea
  ia <buffer> idaes ideas
  ia <buffer> ihs his
  ia <buffer> inot into
  ia <buffer> inteh in the
  ia <buffer> inthe in the
  ia <buffer> inwhich in which
  ia <buffer> isthe is the
  ia <buffer> isze size
  ia <buffer> itis it is
  ia <buffer> itwas it was
  ia <buffer> iused used
  ia <buffer> iwll will
  ia <buffer> iwth with
  ia <buffer> jstu just
  ia <buffer> jsut just
  ia <buffer> knwo know
  ia <buffer> knwon known
  ia <buffer> knwos knows
  ia <buffer> konw know
  ia <buffer> konwn known
  ia <buffer> konws knows
  ia <buffer> kwno know
  ia <buffer> laod load
  ia <buffer> lastr last
  ia <buffer> layed laid
  ia <buffer> liek like
  ia <buffer> liekd liked
  ia <buffer> liev live
  ia <buffer> likly likely
  ia <buffer> ling long
  ia <buffer> liuke like
  ia <buffer> loev love
  ia <buffer> lsat last
  ia <buffer> lveo love
  ia <buffer> lvoe love
  ia <buffer> mcuh much
  ia <buffer> mear mere
  ia <buffer> mial mail
  ia <buffer> mkae make
  ia <buffer> mkaes makes
  ia <buffer> mkea make
  ia <buffer> moeny money
  ia <buffer> mroe more
  ia <buffer> msut must
  ia <buffer> muhc much
  ia <buffer> muts must
  ia <buffer> mysefl myself
  ia <buffer> myu my
  ia <buffer> nad and
  ia <buffer> nkow know
  ia <buffer> nkwo know
  ia <buffer> nmae name
  ia <buffer> nowe now
  ia <buffer> nto not
  ia <buffer> nver never
  ia <buffer> nwe new
  ia <buffer> nwo now
  ia <buffer> ocur occur
  ia <buffer> ofits of its
  ia <buffer> ofthe of the
  ia <buffer> oging going
  ia <buffer> ohter other
  ia <buffer> omre more
  ia <buffer> oneof one of
  ia <buffer> onthe on the
  ia <buffer> onyl only
  ia <buffer> ot to
  ia <buffer> otehr other
  ia <buffer> otu out
  ia <buffer> outof out of
  ia <buffer> owrk work
  ia <buffer> owuld would
  ia <buffer> paide paid
  ia <buffer> peice piece
  ia <buffer> puhs push
  ia <buffer> pwoer power
  ia <buffer> rela real
  ia <buffer> rulle rule
  ia <buffer> rwite write
  ia <buffer> sasy says
  ia <buffer> seh she
  ia <buffer> shoudl should
  ia <buffer> sitll still
  ia <buffer> sleect select
  ia <buffer> smae same
  ia <buffer> smoe some
  ia <buffer> sned send
  ia <buffer> soem some
  ia <buffer> sohw show
  ia <buffer> soze size
  ia <buffer> stnad stand
  ia <buffer> stpo stop
  ia <buffer> syas says
  ia <buffer> ta at
  ia <buffer> tahn than
  ia <buffer> taht that
  ia <buffer> tath that
  ia <buffer> teh the
  ia <buffer> tehir their
  ia <buffer> tehn then
  ia <buffer> tehre there
  ia <buffer> tehy they
  ia <buffer> tghe the
  ia <buffer> tghis this
  ia <buffer> thansk thanks
  ia <buffer> thast that
  ia <buffer> thats that's
  ia <buffer> thatthe that the
  ia <buffer> theh then
  ia <buffer> theri their
  ia <buffer> thgat that
  ia <buffer> thge the
  ia <buffer> thier their
  ia <buffer> thign thing
  ia <buffer> thme them
  ia <buffer> thn then
  ia <buffer> thna than
  ia <buffer> thne then
  ia <buffer> thnig thing
  ia <buffer> thre there
  ia <buffer> thsi this
  ia <buffer> thsoe those
  ia <buffer> thta that
  ia <buffer> thyat that
  ia <buffer> thye they
  ia <buffer> ti it
  ia <buffer> tiem time
  ia <buffer> tihs this
  ia <buffer> timne time
  ia <buffer> tiome time
  ia <buffer> tje the
  ia <buffer> tjhe the
  ia <buffer> tkae take
  ia <buffer> tkaes takes
  ia <buffer> tkaing taking
  ia <buffer> todya today
  ia <buffer> tothe to the
  ia <buffer> towrad toward
  ia <buffer> tthe the
  ia <buffer> ture true
  ia <buffer> twpo two
  ia <buffer> tyhat that
  ia <buffer> tyhe the
  ia <buffer> tyhe they
  ia <buffer> uise use
  ia <buffer> veyr very
  ia <buffer> vrey very
  ia <buffer> waht what
  ia <buffer> wass was
  ia <buffer> watn want
  ia <buffer> weas was
  ia <buffer> wehn when
  ia <buffer> werre were
  ia <buffer> whcih which
  ia <buffer> wherre where
  ia <buffer> whic which
  ia <buffer> whihc which
  ia <buffer> whn when
  ia <buffer> whta what
  ia <buffer> wih with
  ia <buffer> wihch which
  ia <buffer> wiht with
  ia <buffer> willbe will be
  ia <buffer> willk will
  ia <buffer> witha with a
  ia <buffer> withe with
  ia <buffer> withh with
  ia <buffer> witht with
  ia <buffer> withthe with the
  ia <buffer> witn with
  ia <buffer> wiull will
  ia <buffer> wnat want
  ia <buffer> wnats wants
  ia <buffer> woh who
  ia <buffer> wohle whole
  ia <buffer> wokr work
  ia <buffer> woudl would
  ia <buffer> wrod word
  ia <buffer> wroet wrote
  ia <buffer> wrok work
  ia <buffer> wtih with
  ia <buffer> wuould would
  ia <buffer> wya way
  ia <buffer> yaer year
  ia <buffer> yera year
  ia <buffer> yoiu you
  ia <buffer> yoru your
  ia <buffer> youare you are
  ia <buffer> youre you're
  ia <buffer> youve you've
  ia <buffer> yrea year
  ia <buffer> ytou you
  ia <buffer> yuo you
  ia <buffer> yuor your

  " user overrides
  let l:user_dict = a:0 ? a:1 : {}
  for l:item in items(l:user_dict)
    let l:fixed = l:item[0]
    for l:subitem in l:item[1]
      execute 'ia <buffer> ' . l:subitem . ' ' . l:fixed
    endfor
  endfor
endfunction
