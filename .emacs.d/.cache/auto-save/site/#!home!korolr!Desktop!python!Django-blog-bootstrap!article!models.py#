from django.db import models
from ckeditor_uploader.fields import RichTextUploadingField
# Create your models here.
import mptt
from mptt.models import MPTTModel, TreeForeignKey


class Keywords(models.Model):
    class Meta():
        db_table = 'keywords'
        verbose_name_plural = "Ключевые слова"
        verbose_name = "Ключевое слово"

    name = models.TextField(max_length=50, unique=True, verbose_name=u'Search')

    def __str__(self):
        return self.name


class Category(MPTTModel):
    class Meta():
        db_table = 'category'
        verbose_name_plural = "Категории"
        verbose_name = "Категория"
        ordering = ('tree_id', 'level')

    name = models.CharField(max_length=150, verbose_name="Категория")
    parent = TreeForeignKey('self', null=True, blank=True, related_name='children', db_index=True)

    class MPTTMeta:
        order_insertion_py = ['name']

    def __str__(self):
        return self.name


mptt.register(Category, order_insertion_by=['name'])


class Article(models.Model):
    class Meta:
        db_table = 'article'

    article_title = models.CharField('Заголовок', max_length=200)
    article_taxt = RichTextUploadingField('Превью')
    article_text = RichTextUploadingField('Контент')
    article_date = models.DateTimeField('Дата')
    article_likes = models.IntegerField('Лайки', default=0)
    article_image = models.ImageField(null=True, blank=True, upload_to="images/", verbose_name="Изображение")
    category = TreeForeignKey(Category, blank=True, null=True, related_name='cat')

    keywords = models.ManyToManyField(Keywords, related_name="keywords", related_query_name="keyword",
                                      verbose_name=u'Теги')

    def __unicode__(self):
        return self.article_title

    def get_absolute_url(self):
        return "articles/get/%i/" % self.id


class Site(models.Model):
    class Meta:
        db_table = 'site'

    adres = models.TextField('Адрес сайта')
    opisanie = models.TextField('Краткое описание сайта')
    site_image = models.ImageField(null=True, blank=True, upload_to="images/", verbose_name="Изображение")
    about_me = models.TextField('Обо мне')
    about_content = RichTextUploadingField('Контент обо мне')
    about_opisanie = models.TextField('Описание Обо мне')
    about_image = models.ImageField(null=True, blank=True, upload_to="images/", verbose_name="Изображение")
    contact_me = models.TextField('Cotact мне')
    contact_content = RichTextUploadingField('Контент контакты')
    contact_opisanie = models.TextField('Описание контакты')
    contact_image = models.ImageField(null=True, blank=True, upload_to="images/", verbose_name="Изображение контакты")

    def __str__(self):
        return self.adres


class Vkladki(models.Model):
    class Meta:
        db_table = 'vkladki'

    link = models.TextField('Вкладки сайта ссылка')
    nazvanie = models.TextField('Название')

    def __str__(self):
        return self.link


class Genre(MPTTModel):
    name = models.TextField(max_length=50, unique=True)
    parent = TreeForeignKey('self', null=True, blank=True, related_name='children', db_index=True)

    class MPTTMeta:
        order_insertion_by = ['name']
