# project/server/user/forms.py


from flask_wtf import FlaskForm
from wtforms import StringField, PasswordField
from wtforms.validators import DataRequired, Email, Length, EqualTo


class LoginForm(FlaskForm):
    email = StringField('Email Адрес', [DataRequired(), Email()])
    password = PasswordField('Пароль', [DataRequired()])


class RegisterForm(FlaskForm):
    email = StringField(
        'Email Адрес',
        validators=[
            DataRequired(),
            Email(message=None),
            Length(min=6, max=40)
        ]
    )
    password = PasswordField(
        'Пароль',
        validators=[DataRequired(), Length(min=6, max=25)]
    )
    confirm = PasswordField(
        'Подтверждение пароля',
        validators=[
            DataRequired(),
            EqualTo('password', message='Passwords must match.')
        ]
    )
