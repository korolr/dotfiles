# project/server/__init__.py


#################
#### imports ####
#################

import os

from flask import Flask, render_template
from flask_login import LoginManager
from flask_bcrypt import Bcrypt
from flask_debugtoolbar import DebugToolbarExtension
from flask_bootstrap import Bootstrap
from flask_sqlalchemy import SQLAlchemy
from flask.ext.admin import Admin, AdminIndexView
from flask_admin.contrib.sqla import ModelView
from flaskext.markdown import Markdown
from flask.ext.security import current_user


################
#### config ####
################

app = Flask(
    __name__,
    template_folder='../client/templates',
    static_folder='../client/static'
)

class MyAdminIndexView(AdminIndexView):
    def is_accessible(self):
        if current_user.is_admin():
            return current_user.is_authenticated() # This does the trick rendering the view only if the user is authenticated

# Create admin. In this block you pass your custom admin index view to your admin area
admin = Admin(app, 'Admin Area', template_mode='bootstrap3', index_view=MyAdminIndexView())

Markdown(app)

app_settings = os.getenv('APP_SETTINGS', 'project.server.config.DevelopmentConfig')
app.config.from_object(app_settings)


####################
#### extensions ####
####################

login_manager = LoginManager()
login_manager.init_app(app)
bcrypt = Bcrypt(app)
toolbar = DebugToolbarExtension(app)
bootstrap = Bootstrap(app)
db = SQLAlchemy(app)


###################
### blueprints ####
###################

from project.server.user.views import user_blueprint
from project.server.main.views import main_blueprint
app.register_blueprint(user_blueprint)
app.register_blueprint(main_blueprint)




###################
### flask-login ####
###################

from project.server.models import User, Post



admin.add_view(ModelView(User, db.session,  endpoint="users_"))
admin.add_view(ModelView(Post, db.session,  endpoint="post_"))
login_manager.login_view = "user.login"
login_manager.login_message_category = 'danger'


@login_manager.user_loader
def load_user(user_id):
    return User.query.filter(User.id == int(user_id)).first()


########################
#### error handlers ####
########################

@app.errorhandler(401)
def unauthorized_page(error):
    return render_template("errors/401.html"), 401


@app.errorhandler(403)
def forbidden_page(error):
    return render_template("errors/403.html"), 403


@app.errorhandler(404)
def page_not_found(error):
    return render_template("errors/404.html"), 404


@app.errorhandler(500)
def server_error_page(error):
    return render_template("errors/500.html"), 500
