# project/server/main/views.py


#################
#### imports ####
#################

from flask import render_template, Blueprint, request
from project.server.models import Post



################
#### config ####
################

main_blueprint = Blueprint('main', __name__,)


################
#### routes ####
################


@main_blueprint.route('/', methods = ['GET'])
@main_blueprint.route('/index', methods = ['GET', 'POST'])
@main_blueprint.route('/index/<int:page>', methods = ['GET', 'POST'])
def home(page = 1):
    q = request.args.get('q')
    if q:
        # TODO Найти рабочий полнотекстовый поиск для орм
        post = Post.query.filter_by(title=q).all()
        return render_template('main/home_q.html', posts=post)

    post = Post.query.paginate(page, 8, False)
    return render_template('main/home.html', posts=post)

@main_blueprint.route('/cat/<string:name>', methods = ['GET', 'POST'])
def cat(name = ''):
    post = Post.query.filter_by(category=name).all()
    return render_template('main/home_q.html', posts=post, names=name)

@main_blueprint.route('/post/<int:post_id>')
def show_post(post_id):
    return render_template('main/post.html', post=Post.query.get(post_id))


@main_blueprint.route("/about/")
def about():
    return render_template("main/about.html")
